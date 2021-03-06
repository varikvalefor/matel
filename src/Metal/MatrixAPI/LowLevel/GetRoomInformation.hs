{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.GetRoomInformation
-- Description : Metal's stuff what fetches the information of rooms
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- As a result of Metal.MatrixAPI.LowLevel's hugeness,
-- @'getRoomInformation'@ is moved from Metal.MatrixAPI.LowLevel to
-- Metal.MatrixAPI.LowLevel.GetRoomInformation.
module Metal.MatrixAPI.LowLevel.GetRoomInformation (
  getRoomInformation
) where
import Metal.Base;
import Control.Monad;
import Metal.Room;
import Metal.Auth;
import Data.Aeson.Quick;
import Network.HTTP.Simple;
import Control.Concurrent.Async;
import Metal.OftenUsedFunctions;
import qualified Data.Aeson as A;
import Control.Lens hiding ((<.>));
import qualified Metal.Default as Def;
import qualified Data.Aeson.Lens as A;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import Metal.MatrixAPI.LowLevel.RecordCombination;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;

-- | @getRoomInformation@ fetches information regarding the specified
-- Matrix room.
--
-- Information which is fetched includes the encryption information
-- regarding the Matrix room, the topic of the Matrix room, and the
-- display name of the Matrix room.
--
-- = Output
--
-- If the information regarding the specified room is /successfully/
-- grabbed, then this information is returned as a 'Right' 'User.  If
-- some breakage occurs, then a description of this breakage is returned
-- as a 'Left' 'ErrorCode'.
getRoomInformation :: Room
                   -- ^ This argument represents the room which should
                   -- be described.
                   -> Auth
                   -- ^ This thing is the authorisation information
                   -- which is used to run the HTTP queries.
                   -> IO (Either Stringth Room);
getRoomInformation r a = getMembers r a >>= either (pure . Left) evil
  where
  -- \| "@evil@" is an abbreviation of "@evaluate@".
  evil g = foldCombine . (Right r:) . (Right g:) <$> fetchRoomValues
  foldCombine = foldr combine' (Right Def.room)
  --
  combine' = liftM2 combine
  -- \| The term "fetch", as opposed to "get", is used to indicate that
  -- @fetchRoomValues@ just concatenates the outputs of various
  -- functions which directly access the Matrix API and does not
  -- directly access the Matrix API.
  fetchRoomValues = mapConcurrently (\f -> f r a) functions
  functions = [getEncryptionStatus, getTopic, getRoomName];

-- | @getEncryptionStatus r a@ 'Either' returns a 'Def.room' which is
-- modified such that this 'Room' describes the encryption status of the
-- room which is represented by @r@ or returns an 'Errorcode' which
-- explains @getEncryptionStatus@'s failure to return a 'Def.room'
-- value.
getEncryptionStatus :: Room
                    -- ^ This record represents the room whose
                    -- encryption information should be grabbed.
                    -- fetched
                    -> Auth
                    -- ^ This value is the authorisation information
                    -- which is used to run the "@m.room.key@" query.
                    -> IO (Either ErrorCode Room);
getEncryptionStatus room = fmap process <.> rq room "/event/m.room_key"
  where
  process response = case getResponseStatusCode response of
    200 -> Def.room {publicKey = fmap (.! "{content:{session_key}") bd}
    _   -> Def.room
    where bd = A.decode $ BSL.fromStrict $ getResponseBody response;

-- | Assuming that everything goes according to plan, @getMembers r a@
-- is a 'Room' record whose @members@ field is a list of the members
-- of the Matrix room which @r@ represents.
--
-- If something breaks, then a 'Stringth' which describes this breakage
-- is output.
getMembers :: Room
           -- ^ This value represents the room whose members should be
           -- listed.
           -> Auth
           -- ^ This value is the authorisation information which is
           -- used to run the query.
           -> IO (Either ErrorCode Room);
getMembers room = (>>= process) <.> rq room "/members"
  where
  process response = case getResponseStatusCode response of
    200 -> Right Def.room
           -- \^ TODO: Implement this thing.
           --
           -- This "return nothing" thing is added because having the
           -- program break at this point can be a bit inconvenient.
    _   -> Left $ responseToStringth response;

-- | @getTopic r whatevs@ 'Either' returns a 'Room' record whose
-- 'topic' value is the topic of the Matrix room whose MXID is
-- @roomId r@) or returns an 'ErrorCode' which explains
-- @getTopic@'s failure to fetch the topic message.
getTopic :: Room
         -- ^ This thing is a representation of the Matrix room whose
         -- topic should be nabbed.
         -> Auth
         -- ^ This 'Auth' record describes the user which requests the
         -- topic.
         --
         -- The authorisation information is demanded because for all
         -- private rooms, the topic of a private room can be fetched
         -- only if this authorisation information is provided.
         -> IO (Either ErrorCode Room);
getTopic r = fmap process <.> rq r "/state/m.room.topic/"
  where
  process k = Def.room {topic = extractTopic k}
  extractTopic k = getResponseBody k ^? A.key "name" . A._String;

-- | @getRoomName j helljail@ 'Either' (returns a 'Room' record
-- whose 'roomName' value is the display name of the Matrix room
-- whose MXID is @roomId j@) or returns an 'ErrorCode' which
-- explains @getRoomName@'s failure to return a 'Room' record.
getRoomName :: Room
            -- ^ This value describes the room whose display name is
            -- fetched.
            -> Auth
            -- ^ This value is the 'Auth'orisation information of the
            -- user which requests the display name.
            --
            -- This authorisation information is demanded because for
            -- all private rooms, the name of a private room can be
            -- fetched only if this authorisation information is
            -- provided.
            -> IO (Either ErrorCode Room);
getRoomName r = fmap process <.> rq r "/state/m.room.name/"
  where
  process k = Def.room {roomName = extractName k}
  extractName k = getResponseBody k ^? A.key "name" . A._String;

-- | @rq@ sends very specific Matrix HTTP requests.
rq :: Room
   -- ^ This thing represents the room whose information is being
   -- grabbed.
   -> String
   -- ^ The path of the HTTP request is the concatenation of
   -- "@https:\/\/[HOMESERVER]\/\_matrix\/client\/v3\/rooms\/\
   -- [roomId ROOM]\/" and this argument.
   -> Auth
   -- ^ This argument is the authorisation information of the Matrix
   -- user on whose behalf this HTTP request is sent.
   -> IO (Either ErrorCode (Response BS.ByteString));
rq room k = TP.req TP.GET [] querr ""
  where
  querr :: String
  querr = "_matrix/client/v3/rooms/" ++ roomId room ++ k;

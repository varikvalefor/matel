{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.GetRoomInformation
-- Description : Metal's stuff what fetches the information of rooms
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
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
import Data.Maybe;
import Metal.Base;
import Metal.Room;
import Metal.Auth;
import Data.Aeson.Quick;
import Network.HTTP.Simple;
import Control.Concurrent.Async;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Aeson as A;
import Control.Lens hiding ((<.>));
import qualified Metal.Default as Def;
import qualified Data.Aeson.Lens as A;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import Metal.MatrixAPI.LowLevel.RecordCombination;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;

-- | @getRoomInformation room a@ equals a 'Room'-based representation of
-- the Matrix room whose internal Matrix ID is specified within @room@
-- if the "members" API query works.
--
-- @getRoomInformation room a@ otherwise equals a description of the
-- problem which is encountered when the "members" query is sent to the
-- Matrix homeserver.
--
-- The "fetch the room members" portion of @getRoomInformation@ is
-- currently unimplemented and always returns @[]@ for existent rooms.
getRoomInformation :: Room
                   -- ^ The room which should be described
                   -> Auth
                   -- ^ The authorisation information
                   -> IO (Either Stringth Room);
getRoomInformation room a =
  getMembers room a >>= either (return . Left) evaluate
  where
  evaluate :: Room -> IO (Either Stringth Room)
  evaluate g = Right . foldr combine Def.room . (g:) <$> fetchRoomValues
  --
  fetchRoomValues :: IO [Room]
  fetchRoomValues = mapConcurrently (\f -> f room a) functions
  -- \^ The term "fetch", as opposed to "get", is used to indicate that
  -- @fetchRoomValues@ just concatenates the outputs of various
  -- functions which directly access the Matrix API and does not
  -- directly access the Matrix API.
  functions :: [Room -> Auth -> IO Room]
  functions = [getEncryptionStatus, getTopic, getRoomName];

-- | @getEncryptionStatus r a@ returns a @'Def.room'@ which is modified
-- such that this 'Room' represents the encryption status of the room
-- which @r@ represents.
getEncryptionStatus :: Room
                    -- ^ The room whose encryption status should be
                    -- fetched
                    -> Auth
                    -- ^ The authorisation information which is used to
                    -- fetch the encryption status
                    -> IO Room;
getEncryptionStatus room = process <.> rq room "/event/m.room_key"
  where
  process :: Response BS.ByteString -> Room
  process response = case getResponseStatusCode response of
    200 -> Def.room {publicKey = Just $ bd .! "{content:{session_key}"}
    _   -> Def.room
    where
    bd = fromJust $ A.decode $ BSL.fromStrict $
         getResponseBody response;
         -- \^ @fromJust@ is used in favour of a relatively elegant
         -- thing because @fromJust@ should always work here.  If
         -- @fromJust@ does not work, then something has gone horribly,
         -- horribly wrong.

-- | Assuming that everything goes according to plan, @getMembers r a@
-- equals a 'Room' record whose @members@ field is a list of the members
-- of the Matrix room which @r@ represents.
--
-- If something breaks, then a 'Stringth' which describes this breakage
-- is output.
getMembers :: Room
           -- ^ The room whose members should be fetched
           -> Auth
           -- ^ The authorisation information which is used to fetch
           -- the list of members
           -> IO (Either Stringth Room);
getMembers room a = pure $ Right room -- process <.> rq room "/members"
  -- \^ This hack is grody.
  --
  -- This hack is created when @getMembers@ is broken such that
  -- @getMembers@'s HTTP request always returns a 404 error and
  -- @getMembers@ _always_ returns 'Left' values; as a result of this
  -- brokenness, @getMembers@ is currently damn near useless.
  --
  -- A fix should be created... eventually.
  where
  process :: Response BS.ByteString -> Either Stringth Room
  process response = case getResponseStatusCode response of
    200 -> Right Def.room
           -- \^ TODO: Implement this thing.
           --
           -- This "return nothing" thing is added because having the
           -- program break at this point can be a bit inconvenient.
    _   -> Left $ responseToStringth response;

-- | Where @a@ is the authorisation information of the client,
-- @getTopic r a@ fetches the topic message of the Matrix room whose
-- internal Matrix room ID is @roomId r@.  This information is returned
-- as a 'Room' record whose @'topic'@ field is non-default.
--
-- The authorisation information is demanded because for all private
-- rooms, the topic of a private room can be fetched only if this
-- authorisation information is provided.
getTopic :: Room
         -- ^ The room whose topic message is hopefully fetched
         -> Auth
         -- ^ The authorisation information of the user
         -> IO Room;
getTopic r = process <.> rq r "/state/m.room.topic/"
  where
  process :: Response BS.ByteString -> Room
  process k = Def.room {topic = fromMaybe kemo $ extractTopic k}
  --
  extractTopic :: Response BS.ByteString -> Maybe T.Text
  extractTopic k = getResponseBody k ^? A.key "name" . A._String
  --
  kemo :: T.Text
  kemo = error $ "A fairly goofy error is encountered.  The JSON " ++
         "value which the \"m.room.topic\" request returns does " ++
         "NOT contain a \"name\" field.";

-- | @getRoomName r a@ fetches the display name of the Matrix room whose
-- room ID is @roomId r@.  The @'roomName'@ value of the output 'Room'
-- record is used contains the desired information.
--
-- The authorisation information is demanded because for all private
-- rooms, the name of a private room can be fetched only if this
-- authorisation information is provided.
getRoomName :: Room
            -- ^ The room whose display name is nabbed
            -> Auth
            -- ^ The authorisation information
            -> IO Room;
getRoomName r = process <.> rq r "/state/m.room.name/"
  where
  process :: Response BS.ByteString -> Room
  process k = Def.room {roomName = fromMaybe kemo $ extractName k}
  --
  extractName :: Response BS.ByteString -> Maybe T.Text
  extractName k = getResponseBody k ^? A.key "name" . A._String
  --
  kemo :: T.Text
  kemo = error "A fairly goofy error is encountered.  The \
               \\"m.room.name\" request returns a JSON value which \
               \does NOT contain a \"name\" field.";

-- | @rq room k a@ is the response to the authorised HTTP request
-- "GET https:\/\/[@homeserver a@]\/matrix\/\_client\/r0\/rooms\
-- [@roomId room@]\/[@k@]".
rq :: Room
   -- ^ The room which is the subject of the request
   -> String
   -- ^ The "\/whatever" addition to the query
   -> Auth
   -- ^ The user whose authorisation details/homeserver FQDN are used
   -> IO (Response BS.ByteString)
rq room k = TP.req TP.GET [] querr ""
  where
  querr :: String
  querr = "/matrix/_client/r0/rooms/" ++ roomId room ++ k;

{-# LANGUAGE OverloadedStrings #-}

-- | As a result of Metal.MatrixAPI.LowLevel's hugeness,
-- @'getRoomInformation'@ is moved from Metal.MatrixAPI.LowLevel to
-- Metal.MatrixAPI.LowLevel.GetRoomInformation.
module Metal.MatrixAPI.LowLevel.GetRoomInformation (
  getRoomInformation
) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Either;
import Network.HTTP.Simple;
import Control.Concurrent.Async;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import Metal.MatrixAPI.LowLevel.GenerateAuth;
import Metal.MatrixAPI.LowLevel.RecordCombination;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;

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
  getMembers room a >>= \memebears ->
  if isLeft memebears
    then return $ Left $ justLeft memebears
    -- This seemingly meaningless "@Left . justLeft@" statement is used
    -- because GHC otherwise complains that the type of @memebears@ does
    -- not equal the range of @getRoomInformation@.
    else
      -- To avoid using unnecessarily large amounts of bandwidth, these
      -- functions are executed only if @getMembers@ works.
      Right . foldr combine Def.room <$> fetchDiscreteRoomValues
  where
  fetchDiscreteRoomValues :: IO [Room]
  fetchDiscreteRoomValues = mapConcurrently (\f -> f room a) functions
  --
  functions :: [(Room -> Auth -> IO Room)]
  functions = [getEncryptionStatus, getTopic, getRoomName];

-- | @getEncryptionStatus r a@ returns a @'Def.room'@ which is modified
-- to represent the encryption status of the room which @r@ represents.
getEncryptionStatus :: Room
                    -- ^ The room whose encryption status should be
                    -- fetched
                    -> Auth
                    -- ^ The authorisation information which is used to
                    -- fetch the encryption status
                    -> IO Room;
getEncryptionStatus room a = process <$> rq room "/event/m.room.key" a
  where
  process :: Response BS.ByteString -> Room
  process response
    | getResponseStatusCode response == 200 = error $ "TODO: " ++
      "IMPLEMENT THIS THING!"
    | otherwise = Def.room;

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
getMembers room a = process <$> rq room "/members" a
  where
  process :: Response BS.ByteString -> Either Stringth Room
  process response
    | getResponseStatusCode response == 200 = Right Def.room -- TODO: Implement this thing.  This "return nothing" thing is added because having the program break at this point can be a bit inconvenient.
    | otherwise = Left $ responseToStringth response;

-- | Where @a@ is the authorisation information of the client,
-- @getTopic r a@ fetches the topic message of the Matrix room whose
-- internal Matrix room ID is @roomId r@.  This information is returned
-- as a 'Room' record whose @'topic'@ field is non-default.
getTopic :: Room
         -- ^ The room whose topic message is hopefully fetched
         -> Auth
         -- ^ The authorisation information of the user
         -> IO Room;
getTopic r a = process <$> rq r "/m.room.topic" a
  where
  process :: Response BS.ByteString -> Room
  process _ = Def.room {roomName = "THIS THING IS UNIMPLEMENTED!!!"};
  -- TODO: IMPLEMENT THIS BIT CORRECTLY.

-- | @getRoomName r a@ fetches the display name of the Matrix room whose
-- room ID is @roomId r@.  The @'roomName'@ value of the output 'Room'
-- record is used contains the desired information.
getRoomName :: Room
            -- ^ The room whose display name is nabbed
            -> Auth
            -- ^ The authorisation information
            -> IO Room;
getRoomName r a = process <$> rq r "/m.room.name" a
  where
  process :: Response BS.ByteString -> Room
  process _ = Def.room {roomName = "THIS THING IS UNIMPLEMENTED!!!"};
  -- TODO: IMPLEMENT THIS BIT CORRECTLY.

-- | @rq room k a@ is the response to the authorised HTTP request
-- "GET https:\/\/[@homeserver a@]\/matrix\/\_client\/r0\/rooms\
-- [@roomId room@]\/[@k@]".
rq :: Room
   -- ^ The room which is the subject of the request
   -> String
   -- ^ The "\/whatever" addition to the query
   -> Auth
   -- The user whose authorisation details/homeserver FQDN are used
   -> IO (Response BS.ByteString)
rq room k a = generateAuthdRequest uri a >>= httpBS
  where
  uri :: String
  uri = "GET https://" ++ homeserver a ++
    "/matrix/_client/r0/rooms" ++ roomId room ++ k;

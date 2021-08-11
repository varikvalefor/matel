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
  fetchDiscreteRoomValues = mapConcurrently id [
    getEncryptionStatus room a,
    getTopic room a,
    getRoomName room a];

-- | @getEncryptionStatus r a@ describes the encryption status of @r@.
--
-- If @r@ describes an unencrypted room, then
-- @r == return ('False', 'Nothing')@.
--
-- If @r@ describes an encrypted room, then the first value of the
-- output is @'True'@, and the second value contains the public key of
-- the room.
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
-- is a 'Right'-based list of ['User']-based members of @r@.  If
-- something breaks, then a 'Left'-based description of the error is
-- returned.
--
-- Only the @username@ bits of the output list are certainly defined.
getMembers :: Room
           -- ^ The room whose members should be fetched
           -> Auth
           -- ^ The authorisation information which is used to fetch
           -- the list of members
           -> IO (Either Stringth [User]);
getMembers room a = process <$> rq room "/members" a
  where
  process :: Response BS.ByteString -> Either Stringth [User]
  process response
    | getResponseStatusCode response == 200 = Right [] -- TODO: Implement this thing.  This "return nothing" thing is added because having the program break at this point can be a bit inconvenient.
    | otherwise = Left $ responseToStringth response;

-- | Where @a@ is the authorisation information of the client,
-- @getTopic r a@ fetches the topic message of the Matrix room whose
-- internal Matrix room ID is @roomId r@.
getTopic :: Room
         -- ^ The room whose topic message is hopefully fetched
         -> Auth
         -- ^ The authorisation information of the user
         -> IO Room;
getTopic _ _ = return Def.room {topic = "THIS THING IS UNIMPLEMENTED!!!"};
-- TODO: IMPLEMENT THIS BIT CORRECTLY.

-- | @getRoomName r a@ fetches the display name of the Matrix room whose
-- room ID is @roomId r@.
getRoomName :: Room
            -- ^ The room whose display name is nabbed
            -> Auth
            -- ^ The authorisation information
            -> IO Room;
getRoomName r a = process <$> rq r "/m.room.name" a
  where
  process :: BS.ByteString -> Room
  process = Def.room {roomName = "THIS THING IS UNIMPLEMENTED!!!"};
  -- TODO: IMPLEMENT THIS BIT CORRECTLY.

-- | @responseToStringth k@ equals a 'Stringth' which describes the
-- status code of @k@.
responseToStringth :: Response a -> Stringth;
responseToStringth r = T.pack $ "Thus spake the homeserver: " ++
  show (getResponseStatusCode r) ++ ".";

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

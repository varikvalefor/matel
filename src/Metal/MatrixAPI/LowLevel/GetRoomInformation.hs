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
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import Metal.MatrixAPI.LowLevel.GenerateAuth;

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
      getEncryptionStatus >>= \(cryptoStatus, cryptoKey) ->
      getTopic room a >>= \theTopic ->
      getRoomName room a >>= \roomName' ->
      return $ Right Def.room {
        roomId = roomId room,
        isEncrypted = cryptoStatus,
        publicKey = cryptoKey,
        members = justRight memebears,
        roomName = roomName',
        topic = theTopic
      }
  where
  getEncryptionStatus :: IO (Bool, Maybe PublicKey)
  getEncryptionStatus =
    rq room "/event/m.room.key" a >>= return . \response ->
    if getResponseStatusCode response == 200
      then (True, error "TODO: IMPLEMENT THIS THING!")
      else (False, Nothing);

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
getMembers room a =
  rq room "/members" a >>= return . \response ->
  if getResponseStatusCode response == 200
    then Right [] -- TODO: Implement this thing.  This "return nothing" thing is added because having the program break at this point can be a bit inconvenient.
    else Left $ responseToStringth response;

-- | Where @a@ is the authorisation information of the client,
-- @getTopic r a@ fetches the topic message of the Matrix room whose
-- internal Matrix room ID is @roomId r@.
getTopic :: Room
         -- ^ The room whose topic message is hopefully fetched
         -> Auth
         -- ^ The authorisation information of the user
         -> IO Stringth;
getTopic _ _ = return "THIS THING IS UNIMPLEMENTED!!!"
-- TODO: IMPLEMENT THIS BIT CORRECTLY.

-- | @getRoomName r a@ fetches the display name of the Matrix room whose
-- room ID is @roomId r@.
getRoomName :: Room
            -- ^ The room whose display name is nabbed
            -> Auth
            -- ^ The authorisation information
            -> IO HumanReadableName;
getRoomName _ _ = return "THIS THING IS UNIMPLEMENTED!!!";
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

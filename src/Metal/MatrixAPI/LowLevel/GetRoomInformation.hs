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
  getMembers >>= \memebears ->
  if isLeft memebears
    then return $ Left $ justLeft memebears
    -- This seemingly meaningless "@Left . justLeft@" statement is used
    -- because GHC otherwise complains that the type of @memebears@ does
    -- not equal the range of @getRoomInformation@.
    else
      getEncryptionStatus >>= \(cryptoStatus, cryptoKey) ->
      getTopic >>= \theTopic ->
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
    rq "/event/m.room.key" >>= return . \response ->
    if getResponseStatusCode response == 200
      then (True, error "TODO: IMPLEMENT THIS THING!")
      else (False, Nothing)
  --
  getMembers :: IO (Either Stringth [User])
  getMembers =
    rq "/members" >>= return . \response ->
    if getResponseStatusCode response == 200
      then Right [] -- TODO: Implement this thing.  This "return nothing" thing is added because having the program break at this point can be a bit inconvenient.
      else Left $ responseToStringth response
  --
  getTopic :: IO Stringth
  getTopic = return "THIS THING IS UNIMPLEMENTED!!!"
  -- TODO: IMPLEMENT THIS BIT CORRECTLY.
  --
  rq :: String -> IO (Response BS.ByteString)
  rq k = generateAuthdRequest uri a >>= httpBS
    where
    uri :: String
    uri = "GET https://" ++ homeserver a ++
      "/matrix/_client/r0/rooms" ++ roomId room ++ k;

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

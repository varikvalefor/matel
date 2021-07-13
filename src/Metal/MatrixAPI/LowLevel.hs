{-# LANGUAGE OverloadedStrings #-}

-- | 'Metal.MatrixAPI.LowLevel' contains functions which directly access
-- the Matrix API via HTTP requests, as opposed to being abstracted.

module Metal.MatrixAPI.LowLevel where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Data.Either;
import Data.Text.Encoding;
import qualified Data.Aeson as A;
import Network.HTTP.Simple;
import Data.List (elemIndex);
import Metal.Messages.Standard;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | That @stillUnfinishedStayTuned@ exists implies that Matel is
-- currently useless as a Matrix client.
--
-- @stillUnfinishedStayTuned@ is removed when proper Matrix API support
-- is added to Matel.
stillUnfinishedStayTuned :: ();
stillUnfinishedStayTuned = ();

-- For all @('ByteData' z, 'PublicKey' k)@, @encryptWKey z k@ encrypts
-- @z@ with @k@, outputting the resulting ciphertext.
--
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData -> PublicKey -> CipherByteData;
encryptWKey text key = T.pack [];

-- For all @('CipherByteData' z, 'PrivateKey' k)@, @decryptTextWKey z k@
-- decrypts @z@ with @k@, outputting the resulting 'ByteData'-based
-- data.
--
-- @decryptWKey@ is currently nonfunctional.
decryptWKey :: CipherByteData -> PrivateKey -> ByteData;
decryptWKey crip key = T.pack [];

-- | For all 'User' @k@, if @username k@ and @password k@ are
-- set, then @login k@ fetches an authorisation token for Matrix user
-- @k@.
--
-- The 'Right' value of @loginPass k p@ equals the authorisation token
-- which results from signing in to Matrix.  The 'Left' value of
-- @loginPass k p@ exists only if an error is present... and equals a
-- description of such an error.
loginPass :: User -> IO (Either Stringth Stringth);
loginPass user =
  generateRequest >>= httpBS >>= return . responseToLeftRight
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("POST https://" ++ homeserver user ++ "/_matrix/client/r0/login") >>=
    return . setRequestBodyJSON logreq
  --
  logreq :: LoginRequest
  logreq = LoginRequest {
    lrq_type = "m.login.password",
    lrq_identifier = UserIdentifier {
      usident_type = "m.id.user",
      usident_user = username user
    },
    lrq_password = password user,
    lrq_initial_device_display_name = "Matel"
  };

-- | @sendSync@ accesses the Matrix "sync" function.
--
-- @sendSync Nothing g@ runs a parameterless "sync".
-- For all other @k@, @sendSync k g@ sends a "sync" query to Matrix such
-- that the "since" parameter of this query equals @fromJust k@.
--
-- The 'Right' value of @sendSync k g@ equals the authorisation token
-- which results from signing in to Matrix.  The 'Left' value of
-- @loginPass k g@ exists only if an error is present... and equals a
-- description of such an error.
sendSync :: Maybe String -- ^ The desired value of the query's "since" field
         -> User -- ^ The authorisation deets
         -> IO (Either Stringth Stringth);
sendSync since user =
  generateRequest >>= httpBS >>= return . responseToLeftRight
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("GET https://" ++ homeserver user ++ "/_matrix/client/r0/sync") >>=
    return . addRequestHeader "Authorization" (authToken' user) . setRequestBodyLBS syncreq
  --
  syncreq :: BSL.ByteString
  syncreq
    | isNothing since = ""
    | otherwise = fromString $ "{\"since\": \"" ++ fromJust since ++ "\"}"
  --
  fromString :: String -> BSL.ByteString
  fromString = BSL.pack . map (toEnum . fromEnum);

-- | @sendJoinedRooms k@ sends the "joined_rooms" query to the
-- homeserver of @k@, authenticating as @k@.
--
-- The 'Right' value of @sendJoinedRooms k g@ equals the authorisation
-- token which results from signing in to Matrix.  The 'Left' value of
-- @sendJoinedRooms k@ exists only if an error is present... and equals a
-- description of such an error.
--
-- The output 'Room' records are NOT completely filled; only the
-- @roomId@ bits are actually defined.
sendJoinedRooms :: User -> IO (Either Stringth [Room]);
sendJoinedRooms a =
  generateRequest >>= httpBS >>= \response ->
    if getResponseStatusCode response == 200
      then return $ Right $ toRooms $ joined_room $ fromJust $
        A.decode $ BSL.fromStrict $ getResponseBody response
      else return $ Left $ T.pack $ "Thus spake the homeserver: " ++
        (show $ getResponseStatusCode response) ++ "."
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("GET https://" ++ homeserver a ++ "/_matrix/client/r0/joined_rooms") >>=
    return .addRequestHeader "Authorization" (authToken' a)
  --
  fromString :: String -> BSL.ByteString
  fromString = BSL.pack . map (toEnum . fromEnum)
  --
  toRooms :: [String] -> [Room]
  toRooms = map (\k -> Room {roomId = k});


-- | If the response code of @k@ equals @200@, then
-- @responseToLeftRight k@ equals the response body of @k@.
-- @responseToLeftRight k@ otherwise equals a string which contains the
-- status code of @k@.
responseToLeftRight :: Response BS.ByteString -> Either Stringth Stringth;
responseToLeftRight k
  | getResponseStatusCode k == 200 =
    Right $ decodeUtf8 $ getResponseBody k
  | otherwise =
    Left $ T.pack $ "Thus spake the homeserver: " ++
    (show $ getResponseStatusCode k) ++ ".";

-- | @sendTextMessage a b c@ sends a message whose body is @a@ to the
-- Matrix room whose room ID is @b@ via the Matrix account which is
-- described in @c@.
sendTextMessage :: Stringth -- ^ Text what should be sent
                -> Identifier -- ^ Internal Matrix ID of room
                -> User -- ^ Authorisation junk
                -> IO (Maybe ErrorCode);
sendTextMessage body dest user =
  generateRequest >>= httpBS >>= \theResponse ->
    if (getResponseStatusCode theResponse) == 200
      then return Nothing
      else return $ Just $ "Thus spake the homeserver: " ++
        (show $ getResponseStatusCode theResponse) ++ "."
  where
  generateRequest :: IO Request
  generateRequest =
    favoriteNoise >>= \fn ->
    parseRequest ("PUT https://" ++ homeserver user ++ "/_matrix/client/r0/rooms/" ++ dest ++ "/send/m.room.message/" ++ fn) >>=
    return . addRequestHeader "Authorization" (authToken' user) . setRequestBodyLBS sendreq
  --
  sendreq :: BSL.ByteString
  sendreq =
    BSL.append (BSL.append "{\"msgtype\": \"m.text\",\n\"body\": " (BSL.fromStrict $ encodeUtf8 body)) "}"
  --
  favoriteNoise :: IO String
  favoriteNoise = BSL.readFile "/dev/random" >>= return . ("$" ++) . map toEnum . take 64 . filter (`elem` (map fromEnum $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) . map fromEnum . BSL.unpack;

-- | @getRoomInformation room a@ equals a 'Room'-based representation of
-- the Matrix room whose internal Matrix ID is specified within @room@
-- if the "members" query works.
-- @getRoomInformation room a@ otherwise equals a description of the
-- problem which is encountered when the "members" query is sent to the
-- Matrix homeserver.
getRoomInformation :: Room -- ^ The room which should be described
                   -> User -- ^ The user account which requests stuff
                   -> IO (Either Stringth Room);
getRoomInformation room a =
  getEncryptionStatus >>= \(cryptoStatus, cryptoKey) ->
  getMembers >>= \memebears ->
  if isLeft memebears
    then return $ Left $ (\(Left k) -> k) memebears
    else return $ Right $ Room {
      roomId = roomId room,
      isEncrypted = cryptoStatus,
      publicKey = cryptoKey,
      members = justRight memebears
    }
  where
  iD :: String
  iD = roomId room
  --
  getEncryptionStatus :: IO (Bool, Maybe PublicKey)
  getEncryptionStatus =
    rq "/event/m.room.key" >>= \response ->
    if getResponseStatusCode response == 200
      then return (True, error "TODO: IMPLEMENT THIS THING!")
      else return (False, Nothing)
  --
  getMembers :: IO (Either Stringth [User])
  getMembers = error "ass"
    rq "/members" >>= \response ->
    if getResponseStatusCode response == 200
      then error "TODO: Implement this thing."
      else error $ "Thus spake the homeserver: " ++ (show $ getResponseStatusCode response) ++ "."
  --
  rq :: String -> IO (Response BS.ByteString)
  rq k = parseRequest ("GET https://" ++ homeserver a ++ "/_matrix/client/r0/rooms/" ++ roomId room ++ k) >>= httpBS . addRequestHeader "Authorization" (authToken' a);

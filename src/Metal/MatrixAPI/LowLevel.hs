{-# LANGUAGE OverloadedStrings #-}

-- | Metal.MatrixAPI.LowLevel contains functions which directly access
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

-- | For all @('ByteData' z, 'PublicKey' k)@, @encryptWKey z k@ encrypts
-- @z@ with @k@, outputting the resulting ciphertext.
--
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData
            -- ^ The plaintext which should be encrypted
            -> PublicKey
            -- ^ The public key which should be used to encrypt the
            -- plaintext
            -> CipherByteData;
encryptWKey text key = T.pack [];

-- | @decryptTextWKey z k@ decrypts @z@ with @k@, outputting the
-- resulting 'ByteData'-based data.
--
-- @decryptWKey@ is currently nonfunctional.
decryptWKey :: CipherByteData
            -- ^ The ciphertext which should be decrypted
            -> PrivateKey
            -- ^ The private key which is used to decrypt the ciphertext
            -> ByteData;
decryptWKey crip key = T.pack [];

-- | If @username k@ and @password k@ are set, then @login k@ fetches an
-- authorisation token for Matrix user @k@.
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
    | otherwise = fromString $ "{\"since\": \"" ++ fromJust since ++ "\"}";

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
sendTextMessage :: Stringth
                -- ^ The body of the message which should be sent
                -> Identifier
                -- ^ The internal Matrix ID of the room to which the
                -- message should be sent
                -> User
                -- ^ Authorisation junk
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
                   -> User -- ^ The authorisation information
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

-- | Where @a@ is the authorisation information of Matel's user, @i@ is
-- the 3-tuple (USER WHICH SENDS INVITE, STATE KEY OF INVITE, SIGNATURE
-- OF INVITE), and @a@ is a 'Room' whose @roomId@ value is appropriately
-- defined, @sendJoin t i a@ sends the
-- @POST /_matrix/client/r0/rooms/{roomId}/join@ command to Matel's
-- user's homeserver, thereby making Matel's user join the specified
-- room @t@.
--
-- If the command is successful, then the output is Nothing.  The output
-- otherwise equals a terse description of the error.
sendJoin :: Room -- ^ The 'Room' which should be joined
         -> Maybe (User, String, String)
            -- ^ The user which sends the invite, the state key of the
            -- invite, and the signature of the invite, respectively, if
            -- the room is not public -- otherwise, Nothing
         -> User -- ^ The authorisation information of Matel's user
         -> IO (Maybe String);
sendJoin r i a =
  generateRequest >>= httpBS >>= \theResponse ->
  if getResponseStatusCode theResponse == 200
    then return Nothing
    else return $ Just $ "Thus spake the homeserver: " ++
      (show $ getResponseStatusCode theResponse)
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("POST https://" ++ homeserver a ++ "/_matrix/client/r0/rooms/" ++ roomId r ++ "/join") >>=
    return . addRequestHeader "Authorization" (authToken' a) . setRequestBodyLBS joinReq
  --
  joinReq :: BSL.ByteString
  joinReq
    | isNothing i = fromString ""
    | otherwise = fromString $
      "{\n\t" ++
        "\"third_party_signed\": {\n\t\t" ++
          "\"sender\": " ++ show (username inviter) ++ ",\n\t\t" ++
          "\"mxid\": " ++ show (username a) ++ ",\n\t\t" ++
          "\"token\": " ++ show inviteStateKey ++ "\n\t\t" ++
          "\"signatures\": {\n\t\t\t" ++
            show (homeserver inviter) ++ ": {\n\t\t\t\t" ++
              "\"ed25519:0\": " ++ show signature ++ ",\n\t\t\t" ++
            "}\n\t\t" ++
          "}\n\t" ++
        "}\n" ++
      "}"
  inviter :: User
  inviter = maybe User {} (\(a,b,c) -> a) i
  --
  inviteStateKey :: String
  inviteStateKey = maybe "" (\(a,b,c) -> b) i
  --
  signature :: String
  signature = maybe "" (\(a,b,c) -> c) i;

-- | @getDisplayName@ implements the Matrix API's
-- "@GET /_matrix/client/r0/profile/{userId}/displayname@" command.
--
-- The first argument describes the user whose display name should be
-- fetched.  Only the @username@ field is used.
--
-- The second argument describes the user of Matel.  This value is used
-- to determine the FQDN of the server which should be queried.  Because
-- no actual authorisation information is used, only the @homeserver@
-- value must be specified.
--
-- If the query returns a status code of 200, then the resulting
-- @displayname@ is added to the input 'User' value and returned.
--
-- If the query returns a status code of 404, then @getDisplayName@
-- assumes that the user has not set a display name and returns the
-- input thing @k@ such that @displayname k == Nothing@.
getDisplayName :: User -- ^ The user whose display name is output
               -> Auth -- ^ The authorisation information of Matel's
                       -- user, used for determining the server which
                       -- should be contacted
               -> IO (Either String User);
getDisplayName u a =
  generateRequest >>= httpBS >>= \theResponse ->
  if getResponseStatusCode theResponse == 200
    then return $ Right $ u {displayname = dnr_displayname $ fromJust $ A.decode $ BSL.fromStrict $ getResponseBody theResponse}
    else if getResponseStatusCode theResponse == 404
      then return $ Right $ u {displayname = T.pack $ username u}
      else return $ Left $ "Thus spake the homeserver: " ++
        (show $ getResponseStatusCode theResponse) ++ "."
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest $ "GET https://" ++ homeserver a ++ "/_matrix/client/r0/profile/" ++ username u ++ "/displayname";

-- | @kick@ implements the Matrix API's
-- "@POST /_matrix/client/r0/rooms/{roomId}/kick@" command.
--
-- The first argument describes the user which is to be kicked.  Only
-- the @username@ field is used.
--
-- The second argument describes the room from which the user should be
-- removed.  Only the @roomId@ field is used.
--
-- The third argument is the reason for the user's removal.
--
-- The fourth argument is the authorisation information which is used
-- to run the command.
--
-- An error message is provided iff an error is encountered.
kick :: User -- ^ A description of the user which should be "kicked"
     -> Room -- ^ The room from which the user should be removed
     -> String -- ^ The reason for the removal of the user
     -> Auth -- ^ The authorisation information
     -> IO (Maybe String);
kick tarjay rome ree a =
  generateRequest >>= httpBS >>= \theResponse ->
  if getResponseStatusCode theResponse == 200
    then return Nothing
    else return $ Just $ "Thus spake the homeserver: " ++
      (show $ getResponseStatusCode theResponse) ++ "."
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("GET https://" ++ homeserver a ++ "/_matrix/client/r0/rooms/" ++ roomId rome ++ "/kick") >>=
    return . addRequestHeader "Authorization" (authToken' a) . setRequestBodyLBS kickReq
  --
  kickReq :: BSL.ByteString
  kickReq = fromString $
    "{\n\t" ++
      "\"user_id\": " ++ show (username tarjay) ++ ",\n\t" ++
      "\"reason\": " ++ show ree ++ "\n" ++
    "}";

-- | @leave@ implements the Matrix API's
-- "@POST /_matrix/client/r0/rooms/{roomId}/leave@" command.
--
-- The first argument specifies the room which the user leaves.  Only
-- the @roomId@ value must be defined.
--
-- The second argument is the authorisation information which is used to
-- actually leave the room.
--
-- A non-'Nothing' output is given iff an error is encountered.  If such
-- a thing is output, then this output describes such an error.
leave :: Room -- ^ The room which should be left
      -> Auth -- ^ The authorisation information
      -> IO (Maybe String);
leave r a =
  generateRequest >>= httpBS >>= \theResponse ->
  if getResponseStatusCode theResponse == 200
    then return Nothing
    else return $ Just $ "Thus spake the homeserver: " ++
      (show $ getResponseStatusCode theResponse) ++ "."
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("POST https://" ++ homeserver a ++ "/_matrix/client/r0/rooms/" ++ roomId r ++ "/leave") >>=
    return . addRequestHeader "Authorization" (authToken' a);

-- | @fromString x@ is a 'BSL.ByteString' whose content is the content
-- of @x@.
--
-- @fromString@ should be used only within this module.
fromString :: String -> BSL.ByteString;
fromString = BSL.pack . map (toEnum . fromEnum);

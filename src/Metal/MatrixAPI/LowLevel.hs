{-# LANGUAGE OverloadedStrings #-}

-- | Metal.MatrixAPI.LowLevel contains functions which directly access
-- the Matrix API via HTTP requests, as opposed to being abstracted.
--
-- Additionally, the functions of this module do NOT transparently
-- support encryption.
module Metal.MatrixAPI.LowLevel (
  encryptWKey,
  decryptWKey,
  loginPass,
  sync,
  joinedRooms,
  joinedSpaces,
  joinedComms,
  sendTextMessage,
  getRoomInformation,
  join,
  getDisplayName,
  kick,
  leave
) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Data.Either;
import Metal.Space;
import Metal.Community;
import Data.Text.Encoding;
import Network.HTTP.Simple;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Data.Aeson as A;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | @stillUnfinishedStayTuned@ exists only if Matel is useless as a
-- Matrix client.
--
-- @stillUnfinishedStayTuned@ is removed when proper Matrix API support
-- is added to Matel.
stillUnfinishedStayTuned :: ();
stillUnfinishedStayTuned = ();

-- | @encryptWKey z k@ encrypts @z@ with the public key @k@, outputting
-- the resulting ciphertext.
--
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData
            -- ^ The plaintext which should be encrypted
            -> PublicKey
            -- ^ The public key which should be used to encrypt the
            -- plaintext
            -> CipherByteData;
encryptWKey text key = T.pack [];

-- | @decryptWKey z k@ decrypts @z@ with @k@, outputting the
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
loginPass :: Auth
          -- ^ The authorisation information of Matel's user
          -> IO (Either Stringth Stringth);
loginPass user = responseToLeftRight <$> (generateRequest >>= httpBS)
  where
  generateRequest :: IO Request
  generateRequest = setRequestBodyJSON logreq <$> parseRequest uri
  --
  uri :: String
  uri = "POST https://" ++ homeserver user ++ "/_matrix/client/r0/login"
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

-- | @sync@ accesses the Matrix "sync" function, returning the result
-- of this synchronisation.
--
-- @sync Nothing g@ runs a parameterless "sync".
-- For all other @k@, @sync k g@ sends a "sync" query to Matrix such
-- that the "since" parameter of this query equals @fromJust k@.
--
-- The 'Right' value of @sync k g@ equals the raw output of the "sync"
-- command.  The 'Left' value of @sync k g@ exists only if an error is
-- present... and equals a description of such an error.
sync :: Maybe String
     -- ^ The desired value of the query's "since" field
     -> Auth
     -- ^ The authorisation deets
     -> IO (Either Stringth Stringth);
sync since user = responseToLeftRight <$> (generateRequest >>= httpBS)
  where
  generateRequest :: IO Request
  generateRequest =
    setRequestBodyLBS syncreq <$> generateAuthdRequest uri user
  --
  uri :: String
  uri = "GET https://" ++ homeserver user ++ "/_matrix/client/r0/sync"
  --
  syncreq :: BSL.ByteString
  syncreq
    | isNothing since = ""
    | otherwise = fromString $
      "{\"since\": \"" ++ fromJust since ++ "\"}";

-- | @joinedRooms k@ sends the "joined_rooms" query to the homeserver of
-- @k@, authenticating as @k@.
--
-- The 'Right' value of @joinedRooms k g@ equals the authorisation
-- token which results from signing in to Matrix.  The 'Left' value of
-- @joinedRooms k@ exists only if an error is present... and equals a
-- description of such an error.
--
-- The output 'Room' records are NOT completely filled; only the
-- @roomId@ bits are actually defined.
joinedRooms :: Auth -> IO (Either Stringth [Room]);
joinedRooms a = generateRequest >>= httpBS >>= return . processResponse
  where
  processResponse :: Response BS.ByteString -> Either Stringth [Room]
  processResponse response
    | getResponseStatusCode response == 200 = Right $ toRooms $
      joined_room $ fromJust $ A.decode $ BSL.fromStrict $
      getResponseBody response
    | otherwise = Left $ responseToStringth response
  --
  generateRequest :: IO Request
  generateRequest = generateAuthdRequest uri a
  --
  uri :: String
  uri = "GET https://" ++ homeserver a ++
    "/_matrix/client/r0/joined_rooms"
  --
  toRooms :: [String] -> [Room]
  toRooms = map (\k -> Def.room {roomId = k});

-- | @joinedSpaces k@ sends the "not yet implemented" query to the
-- homeserver of @k@, authenticating as @k@.
--
-- The 'Right' value of @joinedRooms k g@ equals a list of the 'Space's
-- which Matel's user has joined. The 'Left' value of @joinedRooms k@
-- exists only if an error is present... and equals a description of
-- this error.
--
-- The output 'Space' records are NOT completely filled; only the
-- @spaceId@ bits are non-default.
joinedSpaces :: Auth
             -- ^ The authorisation information of Matel's user
             -> IO (Either Stringth [Space]);
joinedSpaces a = error "joinedSpaces is unimplemented.";

-- | @joinedComms k@ sends the "not yet implemented" query to the
-- homeserver of @k@, authenticating as @k@.
--
-- The 'Right' value of @joinedComms k g@ equals a list of the Matrix
-- communities which Matel's user has joined. The 'Left' value of
-- @joinedComms k@ exists only if an error is present... and equals a
-- description of this error.
--
-- The output 'Community' records are NOT completely filled; only the
-- @spaceId@ bits are non-default.
joinedComms :: Auth
            -- ^ The authorisation information of Matel's user
            -> IO (Either Stringth [Community]);
joinedComms a = error "joinedComms is unimplemented.";

-- | If the response code of @k@ equals @200@, then
-- @responseToLeftRight k@ equals the response body of @k@.
-- @responseToLeftRight k@ otherwise equals a 'Stringth' which contains
-- the status code of @k@.
responseToLeftRight :: Response BS.ByteString
                    -- ^ The 'Response' whose response code should be
                    -- reported
                    -> Either Stringth Stringth;
responseToLeftRight k
  | getResponseStatusCode k == 200 =
    Right $ decodeUtf8 $ getResponseBody k
  | otherwise = Left $ responseToStringth k;

-- | @sendTextMessage a b c@ sends a message whose body is @a@ to the
-- Matrix room whose room ID is @b@.  This message is sent from the
-- Matrix account which is described in @c@.
sendTextMessage :: Stringth
                -- ^ The body of the message which should be sent
                -> Identifier
                -- ^ The internal Matrix ID of the room to which the
                -- message should be sent
                -> Auth
                -- ^ Authorisation junk
                -> IO (Maybe ErrorCode);
sendTextMessage body dest user = generateRequest >>= httpBS >>= tIOMaybe
  where
  tIOMaybe :: Response BS.ByteString -> IO (Maybe ErrorCode)
  tIOMaybe theResp
    | getResponseStatusCode theResp== 200 = return Nothing
    | otherwise = return $ Just $ T.unpack $ responseToStringth theResp
    -- @return@ is used within @tIOMaybe@ because the alternative
    -- implies having a long line within @sendTextMessage@'s most
    -- high-level definition or wrapping a line; VARIK finds that both
    -- such outcomes are undesirable.
  --
  generateRequest :: IO Request
  generateRequest =
    favoriteNoise >>= \fn ->
    setRequestBodyLBS sendreq <$> generateAuthdRequest ("PUT https://" ++ homeserver user ++ "/_matrix/client/r0/rooms/" ++ dest ++ "/send/m.room.message/" ++ fn) user
  --
  sendreq :: BSL.ByteString
  sendreq =
    "{\n\t" ++
      "\"msgtype\": \"m.text\",\n\t" ++
      "\"body\": \"" ++ BSL.fromStrict (encodeUtf8 body) ++ "\"\n" ++
    "}"
    where
    (++) :: BSL.ByteString -> BSL.ByteString -> BSL.ByteString
    (++) = BSL.append
  --
  favoriteNoise :: IO String
  favoriteNoise = toDesiredBits <$> BSL.readFile "/dev/random"
    where
    toDesiredBits :: BSL.ByteString -> String
    toDesiredBits = ("$" ++) . map toEnum . take 64 .
      filter (`elem` allowedV) . map fromEnum . BSL.unpack
    --
    allowedV :: [Int]
    allowedV = map fromEnum $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'];

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
      return $ Right Def.room {
        roomId = roomId room,
        isEncrypted = cryptoStatus,
        publicKey = cryptoKey,
        members = justRight memebears
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
  rq :: String -> IO (Response BS.ByteString)
  rq k = generateAuthdRequest uri a >>= httpBS
    where
    uri :: String
    uri = "GET https://" ++ homeserver a ++
      "/matrix/_client/r0/rooms" ++ roomId room ++ k;

-- | Where @a@ is the authorisation information of Matel's user, @i@ is
-- the 3-tuple (USER WHICH SENDS INVITE, STATE KEY OF INVITE, SIGNATURE
-- OF INVITE), and @t@ is a 'Room' whose @roomId@ value is appropriately
-- defined, @join t i a@ sends the
-- "@POST \/_matrix\/client\/r0\/rooms\/{roomId}\/join@" command to
-- Matel's user's homeserver, thereby making Matel's user join the
-- specified room @t@.
--
-- If the command is successful, then the output is Nothing.  The output
-- otherwise equals a terse description of the error.
join :: Room
     -- ^ The 'Room' which should be joined
     -> Maybe (User, String, String)
     -- ^ The user which sends the invite, the state key of the invite,
     -- and the signature of the invite, respectively, if the room is
     -- not public -- otherwise, 'Nothing'
     -> Auth
     -- ^ The authorisation information of Matel's user
     -> IO (Maybe String);
join r i a = generateRequest >>= httpBS >>= return . toMaybe
  where
  toMaybe :: Response BS.ByteString -> Maybe String
  toMaybe theResponse
    | getResponseStatusCode theResponse == 200 = Nothing
    | otherwise = Just $ T.unpack $ responseToStringth theResponse
  --
  generateRequest :: IO Request
  generateRequest =
    setRequestBodyLBS joinReq <$> generateAuthdRequest uri a
  --
  uri :: String
  uri = "POST https://" ++ homeserver a ++
    "/_matrix/client/r0/rooms/" ++ roomId r ++ "/join"
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
              "\"ed25519:0\": " ++ show signature ++ "\n\t\t\t" ++
            "}\n\t\t" ++
          "}\n\t" ++
        "}\n" ++
      "}"
      -- Manually creating a JSON query is a bit cheesy.  But at least
      -- the speed of the compilation of this thing is greater than the
      -- speed of the compilation of the Aeson equivalent.
  inviter :: User
  inviter = maybe Def.user (\(a,_,_) -> a) i
  --
  inviteStateKey :: String
  inviteStateKey = maybe "" (\(_,b,_) -> b) i
  --
  signature :: String
  signature = maybe "" (\(_,_,c) -> c) i;

-- | @getDisplayName@ implements the Matrix API's
-- "@GET \/_matrix\/client\/r0\/profile\/{userId}\/displayname@"
-- command.
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
-- input thing @k@ such that @displayname k == username k@.
getDisplayName :: User
               -- ^ The user whose display name is output
               -> Auth
               -- ^ The authorisation information of Matel's user, used
               -- to determine the server which should be contacted
               -> IO (Either String User);
getDisplayName u a = processResponse <$> (generateRequest >>= httpBS)
  where
  generateRequest :: IO Request
  generateRequest = parseRequest $ "GET https://" ++ homeserver a ++
    "/_matrix/client/r0/profile/" ++ username u ++ "/displayname"
  --
  toDispName :: Response BS.ByteString -> Stringth
  toDispName = dnr_displayname . fromJust . A.decode .
               BSL.fromStrict . getResponseBody
  --
  processResponse :: Response BS.ByteString -> Either String User
  processResponse r
    | getResponseStatusCode r == 200 =
      Right Def.user {displayname = toDispName r}
    | getResponseStatusCode r == 404 =
      Right Def.user {displayname = T.pack $ username u}
      -- This "404" thing accounts for users whose display names are
      -- undefined.
    | otherwise = Left $ T.unpack $ responseToStringth r;
      -- This case accounts for all situations which should not occur,
      -- e.g., "this user does not exist" and "yo, the server done
      -- broke".  Such responses should raise "red flags"; something has
      -- gone wrong within this module, or the program which uses this
      -- module is implemented badly.  Alternatively, the homeserver
      -- might just be a piece of crap.

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
kick :: User
     -- ^ A description of the user which should be "kicked"
     -> Room
     -- ^ The room from which the user should be removed
     -> String
     -- ^ The reason for the removal of the user
     -> Auth
     -- ^ The authorisation information
     -> IO (Maybe String);
kick tarjay rome ree a = generateRequest >>= httpBS >>= return . toMaybe
  where
  toMaybe :: Response BS.ByteString -> Maybe String
  toMaybe theResponse
    | getResponseStatusCode theResponse == 200 = Nothing
    | otherwise = Just $ T.unpack $ responseToStringth theResponse
  --
  generateRequest :: IO Request
  generateRequest =
    setRequestBodyLBS kickReq <$> generateAuthdRequest uri a
  --
  uri :: String
  uri = "GET https://" ++ homeserver a ++ "/_matrix/client/r0/rooms/" ++
    roomId rome ++ "/kick"
  --
  kickReq :: BSL.ByteString
  kickReq = fromString $
    "{\n\t" ++
      "\"user_id\": " ++ show (username tarjay) ++ ",\n\t" ++
      "\"reason\": " ++ show ree ++ "\n" ++
    "}";

-- | @leave@ implements the Matrix API's
-- "@POST \/_matrix\/client\/r0/rooms\/{roomId}\/leave@" command.
--
-- The first argument specifies the room which the user leaves.  Only
-- the @roomId@ value must be defined.
--
-- The second argument is the authorisation information which is used to
-- actually leave the room.
--
-- A non-'Nothing' value is output iff an error is encountered.  If such
-- a thing is output, then this output describes such an error.
leave :: Room
      -- ^ The room which should be left
      -> Auth
      -- ^ The authorisation information
      -> IO (Maybe String);
leave r a = generateAuthdRequest uri a >>= httpBS >>= return . toMaybe
  where
  toMaybe :: Response BS.ByteString -> Maybe String
  toMaybe theResponse
    | getResponseStatusCode theResponse == 200 = Nothing
    | otherwise = Just $ T.unpack $ responseToStringth theResponse
  --
  uri :: String
  uri = "POST https://" ++ homeserver a ++
    "/_matrix/client/r0/rooms/" ++ roomId r ++ "/leave";

-- | @responseToStringth k@ equals a 'Stringth' which describes the
-- status code of @k@.
responseToStringth :: Response a -> Stringth;
responseToStringth r = T.pack $ "Thus spake the homeserver: " ++
  show (getResponseStatusCode r) ++ ".";

-- | @fromString x@ is a 'BSL.ByteString' whose content is the content
-- of @x@.
--
-- @fromString@ is used only within this module.
fromString :: String -> BSL.ByteString;
fromString = BSL.pack . map (toEnum . fromEnum);

-- | @generateAuthdRequest@ is used to generate authorised requests for
-- the Matrix API, thereby removing some boilerplate crap.
--
-- The first argument consists of the HTTP command which should be sent,
-- a space, and the URI which is accessed.
--
-- The second argument is an 'Auth' record whose @authToken@ field must
-- be defined.
generateAuthdRequest :: String
                     -- ^ The URI of the request, including "POST",
                     -- "GET", or whatever crap is desired
                     -> Auth
                     -- ^ The user whose authorisation crap should be
                     -- added to the request
                     -> IO Request;
generateAuthdRequest r a = addHeader <$> parseRequest r
  where
  -- This "where" clause is used to avoid having a long line.
  -- Although long lines appear elsewhere within Metal's source code,
  -- the author wishes to avoid having long lines of source code iff
  -- such avoidance is feasible.
  addHeader :: Request -> Request
  addHeader = addRequestHeader "Authorization" (authToken' a);

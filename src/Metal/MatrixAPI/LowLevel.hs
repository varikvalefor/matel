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
  join,
  getDisplayName,
  kick,
  leave,
  ban,
  unban,
  createRoom,
  module Metal.MatrixAPI.LowLevel.Send,
  module Metal.MatrixAPI.LowLevel.GetRoomInformation
) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Metal.Space;
import Control.Lens hiding ((<.>));
import Metal.Community;
import Network.HTTP.Simple;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Aeson as A;
import Metal.MatrixAPI.LowLevel.Send;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.Aeson.Lens as A;
import qualified Metal.Default as Def;
import qualified Data.Aeson.Quick as Q;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import Metal.MatrixAPI.LowLevel.GetRoomInformation;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;
-- I need T.P. for my bunghole!

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

-- | If @username k@ and @password k@ are defined, then @login k@
-- fetches an authorisation token for Matrix user @k@.
--
-- The 'Right' value of @loginPass k p@ equals the authorisation token
-- which results from signing in to Matrix.  The 'Left' value of
-- @loginPass k p@ exists only if an error is present... and equals a
-- description of such an error.
loginPass :: Auth
          -- ^ The authorisation information of Matel's user
          -> IO (Either Stringth Stringth);
loginPass a = responseToLeftRight' <$> TP.req TP.POST querr logreq a
  where
  querr :: String
  querr = "_matrix/client/r0/login"
  --
  responseToLeftRight' :: Response BS.ByteString
                       -> Either Stringth Stringth
  responseToLeftRight' j
    -- J
    | getResponseStatusCode j == 200 = Right $ bodyValue Q..! "{access_token}"
    | otherwise = responseToLeftRight j
    where bodyValue = fromJust $ Q.decode $ BSL.fromStrict $
            getResponseBody j
  logreq :: BSL.ByteString
  logreq = fromString $
    "{\n\t" ++
      "\"type\": \"m.login.password\",\n\t" ++
      "\"identifier\": {\n\t\t" ++
        "\"type\": \"m.id.user\",\n\t\t" ++
        "\"user\": " ++ show (username a) ++ "\n\t" ++
      "},\n\t" ++
      "\"password\": " ++ show (password a) ++ ",\n\t" ++
      "\"initial_device_display_name\": \"Matel\"\n" ++
    "}";

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
sync since a = responseToLeftRight <$> TP.req TP.GET querr syncreq a
  where
  querr :: String
  querr = "_matrix/client/r0/sync"
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
joinedRooms a = processResponse <$> TP.req TP.GET querr "" a
  where
  processResponse :: Response BS.ByteString -> Either Stringth [Room]
  processResponse response
    | getResponseStatusCode response == 200 = Right $ toRooms $
      (Q..! "{joined_rooms}") $ fromJust $ Q.decode $ BSL.fromStrict $
      getResponseBody response
    | otherwise = Left $ responseToStringth response
  --
  querr :: String
  querr = "_matrix/client/r0/joined_rooms"
  --
  toRooms :: [String] -> [Room]
  toRooms = map (\k -> Def.room {roomId = k});

-- | @joinedSpaces k@ sends the "not yet implemented" query to the
-- homeserver of @k@, authenticating as @k@.
--
-- The 'Right' value of @joinedRooms k g@ equals a list of the 'Space's
-- which Matel's user has joined.  The 'Left' value of @joinedRooms k@
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
join r i a = responseToMaybe <$> TP.req TP.POST querr joinReq a
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId r ++ "/join"
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
  inviter = maybe Def.user (\(a',_,_) -> a') i
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
               -- ^ The user whose display name is fetched
               -> Auth
               -- ^ The authorisation information of Matel's user, used
               -- to determine the server which should be contacted
               -> IO (Either String User);
getDisplayName u a = processResponse <$> TP.req TP.GET querr "" a
  where
  querr :: String
  querr = "/_matrix/client/r0/profile/" ++ username u ++ "/displayname"
  --
  toDispName :: Response BS.ByteString -> Stringth
  toDispName = dnr_displayname . fromJust . A.decode .
               BSL.fromStrict . getResponseBody
  --
  processResponse :: Response BS.ByteString -> Either String User
  processResponse r =
    case getResponseStatusCode r of
      200 -> Right Def.user {displayname = toDispName r}
      404 -> Right Def.user {displayname = T.pack $ username u}
      -- This "404" thing accounts for users whose display names are
      -- undefined.
      _   -> Left $ T.unpack $ responseToStringth r;
      -- This case accounts for all situations which should not occur,
      -- e.g., "this user does not exist" and "yo, the server done
      -- broke".  Such responses should raise "red flags"; something has
      -- gone wrong within this module, or the program which uses this
      -- module is implemented badly.  Alternatively, the homeserver
      -- might just be a piece of crap.

-- | @kick@ implements the Matrix API's
-- "@POST \/_matrix\/client\/r0\/rooms\/{roomId}\/kick@" command.
--
-- The first argument describes the user which should be kicked.  Only
-- the @username@ field is used.
--
-- The second argument describes the room from which the user should be
-- removed.  Only the @roomId@ field is used.
--
-- The third argument is the reason for the user's removal, e.g., "Your
-- e-mail addresses offend me."
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
kick tarjay rome m a = responseToMaybe <$> TP.req TP.POST querr kickRq a
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId rome ++ "/kick"
  --
  kickRq :: BSL.ByteString
  kickRq = fromString $
    "{\n\t" ++
      "\"user_id\": " ++ show (username tarjay) ++ ",\n\t" ++
      "\"reason\": " ++ show m ++ "\n" ++
    "}";

-- | @ban@ implements the Matrix API's
-- "@POST \/_matrix\/client\/r0\/rooms\/{roomId}\/ban@" command.
--
-- The first argument describes the user which should be banned.  Only
-- the @username@ field is used.
--
-- The second argument describes the room from which the user should be
-- banned.  Only the @roomId@ field is used.
--
-- The third argument is the reason for the banning of the user, e.g.,
-- "Your e-mail addresses offend me."
--
-- The fourth argument is the authorisation information which is used
-- to run the command.
--
-- An error message is provided iff an error is encountered.
ban :: User
     -- ^ A description of the user which should be banned
     -> Room
     -- ^ The room from which the user should be banned
     -> String
     -- ^ The reason for the banning of the user
     -> Auth
     -- ^ The authorisation information
     -> IO (Maybe String);
ban tarjay rome m a = responseToMaybe <$> TP.req TP.POST querr banReq a
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId rome ++ "/ban"
  --
  banReq :: BSL.ByteString
  banReq = fromString $
    "{\n\t" ++
      "\"user_id\": " ++ show (username tarjay) ++ ",\n\t" ++
      "\"reason\": " ++ show m ++ "\n" ++
    "}";

-- | @unban@ implements the Matrix API's
-- "@POST \/_matrix\/client\/r0\/rooms\/{roomId}\/unban@" command.
--
-- The first argument describes the user which should be unbanned.  Only
-- the @username@ field is used.
--
-- The second argument describes the room from which the user should be
-- unbanned.  Only the @roomId@ field is used.
--
-- The third argument is the authorisation information which is used
-- to run the command.
--
-- An error message is provided iff an error is encountered.
unban :: User
     -- ^ A description of the user which should be banned
     -> Room
     -- ^ The room from which the user should be banned
     -> Auth
     -- ^ The authorisation information
     -> IO (Maybe String);
unban tarjay rome a = responseToMaybe <$> TP.req TP.POST querr unbanRq a
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId rome ++ "/unban"
  --
  unbanRq :: BSL.ByteString
  unbanRq = fromString $
    "{\n\t" ++
      "\"user_id\": " ++ show (username tarjay) ++ "\n" ++
    "}";

-- | @leave@ implements the Matrix API's
-- "@POST \/_matrix\/client\/r0\/rooms\/{roomId}\/leave@" command.
--
-- The first argument specifies the room which the user leaves.  Only
-- the @roomId@ value must be defined.
--
-- The second argument is the authorisation information which is used to
-- actually leave the room.
--
-- If an error is encountered, then a 'Just' 'String' which describes
-- this error is output.  If no error is encountered, then 'Nothing' is
-- returned.
leave :: Room
      -- ^ The room which should be left
      -> Auth
      -- ^ The authorisation information
      -> IO (Maybe String);
leave r a = responseToMaybe <$> TP.req TP.POST querr "" a
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId r ++ "/leave";

-- | @createRoom r a@ attempts to create a Matrix room whose information
-- matches the information of @a@.  If this attempt fails, then a
-- 'Stringth' which describes the failure is returned.  If all goes
-- according to plan, then returned is a 'Room' whose @roomId@ value
-- is the room ID of the newly-created room.
createRoom :: Room
           -- ^ This bit describes the room which should be created.
           -- However, this value is currently unused.
           -> String
           -- ^ This bit is describes whether the room should be private
           -- or public.
           --
           -- This value equals "private" iff the room should be
           -- private.  This value equals "public" iff the room should
           -- be a public room.
           -> Auth
           -- ^ The information which is used to authorise the request
           -> IO (Either String Room);
createRoom r publcty = responseToEither <.> TP.req TP.POST querr bod
  where
  querr :: String
  querr = "_matrix/client/r0/createRoom"
  --
  bod :: BSL.ByteString
  bod = fromString $
    "{\n\t" ++
      "\"visibility\": " ++ show publcty ++ ",\n" ++
      "\"name\": " ++ show (roomName r) ++ ",\n" ++
      "\"topic\": " ++ show (topic r) ++ "\n" ++
    "}"
  --
  responseToEither :: Response BS.ByteString -> Either String Room
  responseToEither resp = case getResponseStatusCode resp of
    200 -> Right Def.room {roomId = roomIdOf $ getResponseBody resp}
    _   -> Left $ fromJust $ responseToMaybe resp
  --
  roomIdOf :: BS.ByteString -> Identifier
  roomIdOf = T.unpack . fromMaybe err . (^? A.key "room_id" . A._String)
  --
  err :: Stringth
  err = error "An unexpected error occurs!  The response code \
        \indicates success... but the response String lacks a \
        \\"room_id\" field.";

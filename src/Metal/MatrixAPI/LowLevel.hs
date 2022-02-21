{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel
-- Description : Low-level interface to the Matrix API
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.MatrixAPI.LowLevel contains functions which directly access
-- the Matrix API via HTTP requests, as opposed to being abstracted.
--
-- Additionally, the functions of this module do NOT transparently
-- support encryption.
--
-- The reader should note that this module is currently unfinished and
-- /CANNOT/ access all functionalities of the Matrix API.
module Metal.MatrixAPI.LowLevel (
  -- * Classes
  --
  -- $classes
  Event(..),
  -- * Authorisation Crap
  --
  -- $authorisation
  loginPass,
  -- * Synchronisation
  sync,
  -- * Membership-Describing Stuff
  --
  -- $membershipDescribe
  joinedRooms,
  joinedSpaces,
  joinedComms,
  -- * Membership-Defining Functions
  --
  -- $membershipDefine
  join,
  kick,
  leave,
  ban,
  unban,
  -- * Display-Related Operations
  --
  -- $digitalDisplay
  getDisplayName,
  -- * Stuff what Creates Stuff
  --
  -- $ createsStuff
  createRoom,
  upload,
  -- * Functions what Send Stuff
  --
  -- $spam
  sendEvent,
  -- * Functions what Hide Stuff
  --
  -- $cryptoShit
  Metal.MatrixAPI.LowLevel.decrypt,
  -- * Functions what Describe Stuff
  --
  -- $genericDescribe
  module Metal.MatrixAPI.LowLevel.GetRoomInformation
) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Metal.Space;
import Metal.Community;
import Metal.Messages.Encrypted;
import Network.HTTP.Simple;
import Metal.Messages.Standard;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Aeson as A;
import Control.Lens hiding ((<.>));
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.Aeson.Lens as A;
import qualified Metal.Default as Def;
import qualified Data.Aeson.Quick as Q;
import qualified Data.ByteString as BS;
import Network.HTTP.Types.URI (urlEncode);
import qualified Data.ByteString.Lazy as BSL;
import Metal.MatrixAPI.LowLevel.GetRoomInformation;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;
-- I need T.P. for my bunghole!

-- $authorisation
--
-- This section of this module contains some functions which perform
-- authorisation-related tasks, e.g., fetching new authorisation tokens.

-- | @loginPass@ generates a new authorisation token for Matel's user.
--
-- = Output
--
-- If an authorisation token is successfully generated, then this
-- authorisation token is returned as a 'Right' 'Stringth'.
--
-- If something fails such that no authorisation token can be nabbed,
-- then a 'Left' 'ErrorCode' which describes this failure is returned.
loginPass :: Auth
          -- ^ This bit is the authorisation information of the user for
          -- which an authorisation token is generated.
          -> IO (Either ErrorCode Stringth);
loginPass a = responseToLeftRight' <$> TP.req TP.POST [] querr logreq a
  where
  querr = "_matrix/client/r0/login"
  responseToLeftRight' j = case getResponseStatusCode j of
    200 -> mayB2Eit $ (Q..! "{access_token}") <$> bodyValue j
    _   -> responseToLeftRight j
  mayB2Eit = maybe (Left invalidBodyMsg) Right
  invalidBodyMsg = "loginPass: The body of the response cannot be \
                   \parsed as valid JSON."
  bodyValue = Q.decode . BSL.fromStrict . getResponseBody
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
-- = Output
--
-- If everything goes according to plan, then the raw body of the "sync"
-- response is 'Right'ly returned.  Otherwise, a description of some
-- breakage is returned as a 'Left' 'Stringth'.
sync :: Maybe String
     -- ^ If a "since" value should be attached, then this value is
     -- 'Just' this "since" value.  This value should otherwise be
     -- 'Nothing'.
     -> Auth
     -- ^ This argument is /still/ just authorisation stuff.
     -> IO (Either Stringth Stringth);
sync since = responseToLeftRight <.> TP.req TP.GET [] querr syncreq
  where
  querr = "_matrix/client/r0/sync"
  syncreq = maybe "" encapsulate since
  encapsulate teavea = fromString $ "{\"since\": \"" ++ teavea ++ "\"}";

-- $membershipDescribe
--
-- This section of this module contains functions which fetch lists of
-- objects of which Matel's user is a member.
--
-- Although this module is called \"Metal.MatrixAPI.LowLevel\", most
-- functions which are contained within this section should be
-- /reasonably/ high-level.

-- | @joinedRooms@ returns a list of 'Room's of which Matel's user is a
-- member... or an 'ErrorCode'.
--
-- = Output
--
-- If the fetching of the list of spaces works fine, then this list of
-- 'Room's is 'Right'ly returned.  Otherwise, a 'Left' ErrorCode' which
-- describes the problem which occurs is returned.
--
-- = Notes
--
-- The output 'Room' records are NOT completely filled; only the
-- @roomId@ bits are actually defined.
joinedRooms :: Auth
            -- ^ This bit is the authorisation information of the user
            -- whose joined rooms are listed.
            -> IO (Either ErrorCode [Room]);
joinedRooms = processResponse <.> TP.req TP.GET [] querr ""
  where
  toEither = maybe (Left "joinedRooms: Decoding fails!") Right
  maybeRooms = (map toRoom . (Q..! "{joined_rooms}")) <.> Q.decode
  querr = "_matrix/client/r0/joined_rooms"
  toRoom k = Def.room {roomId = k}
  processResponse r = case getResponseStatusCode r of
    200 -> toEither $ maybeRooms $ BSL.fromStrict $ getResponseBody r
    _   -> Left $ responseToStringth r;

-- | @joinedSpaces@ fetches a list of the 'Space's of which Matel's user
-- is a member.
--
-- = Output
--
-- If the fetching of the list of spaces works fine, then this list of
-- 'Space's is 'Right'ly returned.  Otherwise, a 'Left' 'ErrorCode'
-- which describes the problem which occurs is returned.
--
-- = Notes
--
-- The output 'Space' records are NOT completely filled; only the
-- @spaceId@ bits are non-default.
joinedSpaces :: Auth
             -- ^ This argument is the authorisation information of the
             -- user whose joined spaces are listed.
             -> IO (Either ErrorCode [Space]);
joinedSpaces _ = pure $ Left "joinedSpaces is unimplemented.";

-- | @joinedComms@ fetches a list of the 'Community's -- eugh -- of
-- which Matel's user is a member.
--
-- = Output
--
-- If the fetching of the list of spaces works fine, then this list of
-- 'Commnunity's -- again, eugh -- is 'Right'ly returned.  Otherwise, a
-- 'Left' ErrorCode' which describes the problem which occurs is
-- returned.
--
-- = Notes
--
-- The output 'Space' records are NOT completely filled; only the
-- @commId@ bits are non-default.
joinedComms :: Auth
            -- ^ This value is the authorisation information of the user
            -- whose joined communities are listed.
            -> IO (Either ErrorCode [Community]);
joinedComms _ = pure $ Left "joinedComms is unimplemented.";

-- $membershipDefine
--
-- This section of the module contains functions which define whether or
-- not arbitrary Matrix users are members of arbitrary Matrix objects,
-- e.g., rooms and spaces.

-- | @join@ is used to join Matrix rooms.
--
-- = Output
--
-- If the command is successful, then the output is Nothing.  The output
-- otherwise equals a terse description of the error.
join :: Room
     -- ^ The 'Room' which should be joined
     -> Maybe (User, String, String)
     -- ^ If the room which should be joined is public, then this value
     -- should be 'Nothing'.
     --
     -- If the room which should be joined is /private/, then this value
     -- is 'Just' a 3-tuple @(a,b,c)@, where @a@ is a description of the
     -- user which sends an (invite to the room) @bk@ to the
     -- authenticated @b@ is the state key of the aforementioned invite,
     -- and @c@ is the signature of the aforementioned invite.
     -> Auth
     -- ^ This value is the authorisation information of the user which
     -- joins the specified room.
     -> IO (Maybe ErrorCode);
join r i a = responseToMaybe <$> TP.req TP.POST [] querr joinReq a
  where
  querr = "_matrix/client/r0/rooms/" ++ roomId r ++ "/join"
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
  inviter = maybe Def.user (\(a',_,_) -> a') i
  inviteStateKey = maybe "" (\(_,b,_) -> b) i
  signature = maybe "" (\(_,_,c) -> c) i;

-- | @kick@ non-permanently removes users from Matrix rooms.
--
-- = Output
--
-- If an error is encountered, then a description of this error is
-- 'Just' returned.  Otherwise, an IO-monadic 'Nothing' is output.
kick :: User
     -- ^ This thing represents the user which is to be kicked.  Only
     -- the @username@ field of this record is used.
     -> Room
     -- ^ This value is a representation of the Matrix room from which
     -- the specified user is removed.  Only the @roomId@ value is used.
     -> String
     -- ^ This bit is the reason for the removal of the user, e.g.,
     -- "[y]our e-mail addresses offend me."
     -> Auth
     -- ^ The final argument is the authorisation information of the
     -- user which attempts to kick the other user.
     -> IO (Maybe ErrorCode);
kick tarjay rome m = responseToMaybe <.> TP.req TP.POST [] querr kickRq
  where
  querr = "_matrix/client/r0/rooms/" ++ roomId rome ++ "/kick"
  kickRq = fromString $ unwords ["{", st_user_id, ",", st_reason, "}"]
  st_user_id = "\"user_id\": " ++ show (username tarjay)
  st_reason = "\"reason\": " ++ show m;

-- | @ban@ "permanently" removes Matrix users from Matrix rooms.
--
-- = Output
--
-- If some error, e.g., "hey, lard-ass, you can't ban the admin if you
-- yourself are banned", is encountered, then a description of this
-- error is 'Just' output.  Otherwise, 'Nothing' is returned.
ban :: User
    -- ^ This value represents the user which should be banned.
    -- @username@ is the only value which is used.
    -> Room
    -- ^ This record represents the room from which the user is banned.
    -- @roomId@ is the only value which is used.
    -> String
    -- ^ This bit is the justification for the banning of the user,
    -- e.g., "this dude killed me".
    -> Auth
    -- ^ This argument is the authorisation information of the user
    -- which kicks the other user.
    -> IO (Maybe ErrorCode);
ban tarjay rome m = responseToMaybe <.> TP.req TP.POST [] querr banReq
  where
  querr = "_matrix/client/v3/rooms/" ++ roomId rome ++ "/ban"
  banReq = fromString $ unwords ["{", st_user_id, ",", st_reason, "}"]
  st_user_id = "\"user_id\":" ++ show (username tarjay)
  st_reason = "\"reason\": " ++ show m;

-- | @unban@ reverses users' being @'ban'@ned.
--
-- = Output
--
-- If some error, e.g., "yo, tubby, you can't un-ban yourself", is
-- encountered, then a description of this error is 'Just' output.
-- Otherwise, 'Nothing' is returned.
unban :: User
      -- ^ This argument represents the Matrix user which should be
      -- un-banned.  @username@ is the only value which is used.
      -> Room
      -- ^ This bit represents the Matrix room from which the
      -- aforementioned Matrix user should be un-banned. @roomId@ is
      -- the only value which is used.
      -> Auth
      -- ^ This record is the authorisation information of the user
      -- which un-bans the other user.
      -> IO (Maybe ErrorCode);
unban tarjay rome = responseToMaybe <.> TP.req TP.POST [] querr unbanRq
  where
  querr = "_matrix/client/r0/rooms/" ++ roomId rome ++ "/unban"
  unbanRq = fromString ur'
  ur' = unwords ["{", "\"user_id\":", show $ username tarjay, "}"];

-- | @leave@ is used to leave Matrix rooms.
--
-- = Output
--
-- If an error is encountered, then an 'ErrorCode' which describes
-- this error is 'Just' output.  If no error is encountered, then
-- 'Nothing' is returned.
leave :: Room
      -- ^ This argument represents the room which the user should
      -- leave.  @roomId@ is the only value which is used.
      -> Auth
      -- ^ This bit is the authorisation information of the user which
      -- leaves the room.
      -> IO (Maybe ErrorCode);
leave lamersPalace = responseToMaybe <.> TP.req TP.POST [] querr ""
  where
  querr = "_matrix/client/r0/rooms/" ++ roomId lamersPalace ++ "/leave";

-- $digitalDisplay
--
-- This section of this module contains functions which do things which
-- are primarily display-related, as opposed to being totally
-- utilitarian.

-- | @getDisplayName@ implements the Matrix API's
-- "@GET \/_matrix\/client\/r0\/profile\/{userId}\/displayname@"
-- command, fetching the username of the specified 'User'.
--
-- = Output
--
-- == 'Right' Values
--
-- If the query returns a status code of 200, then the resulting
-- @displayname@ is added to the input 'User' value and returned.
--
-- If the query returns a status code of 404, then @getDisplayName@
-- assumes that the user has not set a display name and returns the
-- input thing @k@ such that @displayname k == username k@.
--
-- == 'Left' Values
--
-- Where K denotes the set of all errors which @getDisplayName@ can
-- encounter, for all l in K, @getDisplayName@ encounters l iff a
-- 'Left' 'String' which describes l is output.
getDisplayName :: User
               -- ^ This argument describes the user whose display name
               -- should be fetched.  @username@ is the only field which
               -- is actually used.
               -> Auth
               -- ^ This argument describes the user of Matel.
               --
               -- This value is used to determine the FQDN of the server
               -- which should be queried.  Because no actual
               -- authorisation information is needed, @homeserver@ is
               -- the only field which is actually used.
               -> IO (Either ErrorCode User);
getDisplayName u = processResponse <.> TP.req TP.GET [] querr ""
  where
  toEither = maybe (Left failedDecodeMsg) Right
  failedDecodeMsg = "getDisplayName: The decoding process fails."
  querr = "/_matrix/client/r0/profile/" ++ username u ++ "/displayname"
  --
  toDispName :: Response BS.ByteString -> Either ErrorCode Stringth
  toDispName = toEither . (dnr_displayname <.> A.decode) .
               BSL.fromStrict . getResponseBody
  --
  processResponse r = case getResponseStatusCode r of
    200 -> (\j -> Def.user {displayname = j}) <$> toDispName r
    -- \| This "404" thing accounts for users whose display names are
    -- undefined.
    404 -> Right Def.user {displayname = T.pack $ username u}
    -- \| This case accounts for all situations which SHOULD NOT occur,
    -- e.g., "this user does not exist" and "yo, the server done
    -- broke".  Such responses should raise "red flags"; something has
    -- gone wrong within this module, or the program which uses this
    -- module is implemented poorly.  Alternatively, the homeserver
    -- might just be a piece of crap.
    _   -> Left $ responseToStringth r;

-- $createsStuff
--
-- This section of the module contains some functions which are used to
-- create or -- or just publish -- objects on Matrix.

-- | @createRoom@ creates new Matrix rooms.
--
-- = Output
--
-- If all goes well, then a 'Right' 'Room' value whose @roomId@ is the ID
-- of the new room is returned.
--
-- If something 'splodes, then a 'Left' 'ErrorCode' which describes the
-- 'splosion is returned.
createRoom :: Room
           -- ^ This bit describes the room which should be created.
           -- The @roomName@ and @topic@ values SHOULD be defined... but
           -- are technically not required.
           -> String
           -- ^ This bit describes whether the room should be private or
           -- public.
           --
           -- If this value is "public", then a public room is created.
           --
           -- If this value is "private", then a private room is
           -- created.
           --
           -- If this value is some other thing, then @createRoom@
           -- probably just 'splodes.
           -> Auth
           -- ^ This bit is the authorisation information of the account
           -- which creates the new room.
           -> IO (Either ErrorCode Room);
createRoom r publcty = responseToEither <.> TP.req TP.POST [] querr bod
  where
  querr = "_matrix/client/r0/createRoom"
  bod = fromString $ unwords ["{", visStat, namStat, topStat, "}"]
  --
  visStat = "\"visibility\": " ++ show publcty
  namStat = maybeKVP "name" roomName
  topStat = maybeKVP "topic" topic
  --
  maybeKVP jf fc = maybe "" ((" ," ++) . toKVP jf) (fc r)
  --
  toKVP :: String -> T.Text -> String
  toKVP header vl = show header ++ ": " ++ show vl
  --
  responseToEither resp = case getResponseStatusCode resp of
    200 -> roomWithId <$> roomIdOf (getResponseBody resp)
    _   -> Left $ responseToStringth resp
  roomWithId rid = Def.room {roomId = rid}
  roomIdOf = toEither . (T.unpack <.> (^? A.key "room_id" . A._String))
    where toEither = maybe (Left err) Right
  err = "An unexpected error occurs!  The response code indicates \
        \success... but the body of the response lacks a \"room_id\" \
        \field.\nThe homeserver could have broken spectacularly, or \
        \createRoom could contain an error.";

-- | @upload@ uploads files to the homeserver of Matel's user.
--
-- = Output
--
-- If the uploading is a success, then the MXC URI of the uploaded file
-- is 'Right'ly output.
--
-- If the output is a failure for any reason, then a 'Left' 'ErrorCode'
-- which hopefully explains this failure is returned.
--
-- = On Using 'BSL.ByteString'
--
-- 'BSL.ByteString' is used instead of 'T.Text' because weird binary
-- files sometimes make 'T.Text' blow up, which is a bit annoying.
--
-- VARIK suspects that 'T.Text'\'s problem is caused by fancy UTF-8
-- parsing which attempts to interpret some nonsense as UTF-8 text,
-- which leads to parsing errors, which lead to the \'splosions.
--
-- This problem is not really the fault of 'T.Text', as 'T.Text' is not
-- meant to read binary files -- 'T.Text' contains /text/, as opposed to
-- /strings of bytes/; read the name, foo' -- rather, this problem is
-- the fault of the author of @upload@.
--
-- PROTIP: Using the most fitting tools prevents a decent number of
-- problems.
upload :: BSL.ByteString
       -- ^ This bit is the content of the file which is to be uploaded.
       -> String
       -- ^ This argument is the name of the file which is to be
       -- uploaded.  This argument can be the current name of the file
       -- or just the desired name of the file.
       -> Auth
       -- ^ This argument is -- as the reader hopefully guessed -- the
       -- authorisation garbage which is used to actually upload the
       -- file.
       -> IO (Either ErrorCode Stringth);
upload attachment name = process <.> TP.req TP.POST hdr qq attachment
  where
  process :: Response BS.ByteString -> Either ErrorCode Stringth
  process k = case getResponseStatusCode k of
    200 -> pj $ Q.decode $ BSL.fromStrict $ getResponseBody k
    _   -> responseToLeftRight k
  -- \| "pj" is an abbreviation of "procJSON".
  pj :: Maybe Q.Value -> Either ErrorCode Stringth
  pj = maybe noBody (maybe badCUri Right . (Q..! "{content_uri}"))
  --
  noBody = Left "upload: The JSON response lacks a valid \"body\" \
                \field."
  badCUri = Left "upload: The response body lacks a valid \
                 \\"content_uri\" field."
  hdr = [("Content-Type", "text/plain")]
  qq = "_matrix/media/r0/upload?filename=" ++
       toString (urlEncode True $ fromString name);

-- $spam
--
-- This section of the module contains the functions and whatnot which
-- facilitate sending stuff to Matrix rooms.

-- | @sendEvent@ sends the specified 'Event' to the specified Matrix
-- room.
--
-- = Output
--
-- If the message is successfully sent, then 'Nothing' is returned.
-- Otherwise, a description of whatever problem @sendEvent@ encounters
-- is 'Just'ly returned.
sendEvent :: Event a
          => A.ToJSON a
          => a
          -- ^ This value is the 'Event' which should be sent.
          -> Room
          -- ^ This value is a representation of the Matrix room to
          -- which the aforementioned 'Event' should be sent.
          -> Auth
          -- ^ This value is the authorisation information which is
          -- used to actually send the 'Event'.
          -> IO (Maybe ErrorCode);
sendEvent ev rm a = qenerateQuery >>= sendQuery
  where
  sendQuery querr = process <$> TP.req TP.PUT [] querr (A.encode ev) a
  qenerateQuery = (("_matrix/client/r0/rooms/" ++ roomId rm ++
                  "/send/" ++ eventType ev ++ "/") ++) <$> favoriteNoise
  process k = case getResponseStatusCode k of
    200 -> Nothing
    _   -> Just $ "sendEvent: " `T.append` responseToStringth k;
-- $cryptoShit
--
-- This section of the module contains functions which directly
-- interface with Matrix's cryptographic protocols.

-- | @decrypt@ decrypts Matrix messages.
--
-- = Output
--
-- If the input 'Encrypted' message is successfully decrypted, then the
-- decrypted 'StdMess' is 'Right'ly output.
--
-- If something fails, then a description of this failure is output as a
-- 'Left' 'ErrorCode'.
--
-- = Meat and Potatoes
--
-- Keep looking.  @decrypt@ just selects and runs an appropriate
-- decryption function; "true" decryption logic is /not/ contained
-- within the definition of @decrypt@.
decrypt :: Auth
        -- ^ This value contains the authorisation information of the
        -- user for whom the input message is encrypted.
        -> Encrypted
        -- ^ This record is the message which is to be decrypted.
        -> Either ErrorCode StdMess;
decrypt _ _ = Left "decrypt is unimplemented.";

-- $genericDescribe
--
-- This section of the module contains functions which describe things
-- such that these descriptions can be used for utilitarian purposes, as
-- opposed to being purely display-related.

-- $classes
--
-- This section contains classes which are relevant only to this module.

-- | Types whose values represent Matrix events belong to 'Event'.
class Event a where
  -- | @eventType k@ is a Matrix-friendly representation of the event
  -- type of @k@, e.g., @"m.message"@.
  eventType :: a -> String;

instance Event StdMess where
  eventType _ = "m.room.message";

instance Event Encrypted where
  eventType _ = "m.room.encrypted";

{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel
-- Description : Low-level interface to the Matrix API
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.MatrixAPI.LowLevel contains functions which directly access
-- the Matrix API via HTTP requests, as opposed to being abstracted.
--
-- Additionally, the functions of this module do NOT transparently
-- support encryption.
module Metal.MatrixAPI.LowLevel (
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
  module Metal.MatrixAPI.LowLevel.Send,
  -- * Functions what Hide Stuff
  --
  -- $cryptoShit
  module Metal.MatrixAPI.LowLevel.Crypto,
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
import Network.HTTP.Simple;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import Network.HTTP.Types.Header;
import qualified Data.Aeson as A;
import Control.Lens hiding ((<.>));
import Metal.MatrixAPI.LowLevel.Send;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.Aeson.Lens as A;
import qualified Metal.Default as Def;
import qualified Data.Aeson.Quick as Q;
import qualified Data.ByteString as BS;
import Metal.MatrixAPI.LowLevel.Crypto;
import Network.HTTP.Types.URI (urlEncode);
import qualified Data.ByteString.Lazy as BSL;
import Metal.MatrixAPI.LowLevel.GetRoomInformation;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;
-- I need T.P. for my bunghole!

-- | @stillUnfinishedStayTuned@ exists only if Matel is useless as a
-- Matrix client.
--
-- @stillUnfinishedStayTuned@ is removed when proper Matrix API support
-- is added to Metal.
stillUnfinishedStayTuned :: ();
stillUnfinishedStayTuned = ();

-- $authorisation
--
-- This section of this module contains some functions which perform
-- authorisation-related tasks, e.g., fetching new authorisation tokens.

-- | @login@ generates a new authorisation token for Matel's user.
--
-- = Arguments
--
-- The first and only argument is an 'Auth' record whose @username@ and
-- @password@ fields must be defined and valid.  An authorisation token
-- is hopefully generated for the account which this 'Auth' record
-- describes.
--
-- = Output
--
-- If an authorisation token is successfully generated, then this
-- authorisation token is returned as a 'Right' 'Stringth'.
--
-- If something fails such that no authorisation token can be nabbed,
-- then a 'Left' 'ErrorCode' which describes this failure is returned.
loginPass :: Auth
          -- ^ The authorisation information of Matel's user
          -> IO (Either ErrorCode Stringth);
loginPass a = responseToLeftRight' <$> TP.req TP.POST [] querr logreq a
  where
  querr :: String
  querr = "_matrix/client/r0/login"
  --
  responseToLeftRight' :: Response BS.ByteString
                       -> Either ErrorCode Stringth
  responseToLeftRight' j = case getResponseStatusCode j of
    200 -> Right $ bodyValue j Q..! "{access_token}"
    _   -> responseToLeftRight j
  --
  bodyValue :: Response BS.ByteString -> Q.Value
  bodyValue = fromJust . Q.decode . BSL.fromStrict . getResponseBody
  --
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
-- = Arguments
--
-- If 'Just', the first argument is the "since" parameter of the "sync"
-- request which is sent to the Matrix homeserver.  If this bit is
-- 'Nothing', then no "since" parameter is sent.
--
-- The second argument is _still_ just authorisation stuff.
--
-- = Output
--
-- If everything goes according to plan, then the raw body of the "sync"
-- response is 'Right'ly returned.  Otherwise, a description of some
-- breakage is returned as a 'Left' 'Stringth'.
sync :: Maybe String
     -- ^ The desired value of the query's "since" field
     -> Auth
     -- ^ The authorisation deets
     -> IO (Either Stringth Stringth);
sync since = responseToLeftRight <.> TP.req TP.GET [] querr syncreq
  where
  querr :: String
  querr = "_matrix/client/r0/sync"
  --
  syncreq :: BSL.ByteString
  syncreq = maybe "" encapsulate since
  --
  encapsulate :: String -> BSL.ByteString
  encapsulate teavea = fromString $ "{\"since\": \"" ++ teavea ++ "\"}";

-- $membershipDescribe
--
-- This section of this module contains functions which fetch lists of
-- objects of which Matel's user is a member.
--
-- Although this module is called \"Metal.MatrixAPI.LowLevel\", most
-- functions which are contained within this section should be
-- _reasonably_ high-level.

-- | @joinedRooms@ returns a list of which Matel's user is a member...
-- or an 'ErrorCode'.
--
-- = Arguments
--
-- The first and only argument is the authorisation information of
-- Matel's user.
--
-- = Output
-- If the fetching of the list of spaces works fine, then this list of
-- 'Room's is 'Right'ly returned.  Otherwise, a 'Left' ErrorCode' which
-- describes the problem which occurs is returned.
--
-- = Notes
--
-- The output 'Room' records are NOT completely filled; only the
-- @roomId@ bits are actually defined.
joinedRooms :: Auth -> IO (Either ErrorCode [Room]);
joinedRooms = processResponse <.> TP.req TP.GET [] querr ""
  where
  processResponse :: Response BS.ByteString -> Either Stringth [Room]
  processResponse r = case getResponseStatusCode r of
    200 -> Right $ extractRooms $ BSL.fromStrict $ getResponseBody r
    _   -> Left $ responseToStringth r
  --
  extractRooms :: BSL.ByteString -> [Room]
  extractRooms = map toRoom . (Q..! "{joined_rooms}") . fromJust . Q.decode
  --
  querr :: String
  querr = "_matrix/client/r0/joined_rooms"
  --
  toRoom :: String -> Room
  toRoom k = Def.room {roomId = k};

-- | @joinedSpaces@ fetches a list of the 'Space's of which Matel's user
-- is a member.
--
-- = Arguments
--
-- The first and only argument is the authorisation information of
-- Matel's user.
--
-- = Output
--
-- If the fetching of the list of spaces works fine, then this list of
-- 'Space's is 'Right'ly returned.  Otherwise, a 'Left' 'ErrorCode' which
-- describes the problem which occurs is returned.
--
-- = Notes
--
-- The output 'Space' records are NOT completely filled; only the
-- @spaceId@ bits are non-default.
joinedSpaces :: Auth
             -- ^ The authorisation information of Matel's user
             -> IO (Either ErrorCode [Space]);
joinedSpaces a = error "joinedSpaces is unimplemented.";

-- | @joinedComms@ fetches a list of the 'Community's -- eugh -- of
--- which Matel's user is a member.
--
-- = Arguments
--
-- The first and only argument is the authorisation information of
-- Matel's user.
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
            -- ^ The authorisation information of Matel's user
            -> IO (Either ErrorCode [Community]);
joinedComms a = error "joinedComms is unimplemented.";

-- $membershipDefine
--
-- This section of the module contains functions which define whether or
-- not arbitrary Matrix users are members of arbitrary Matrix objects,
-- e.g., rooms and spaces.

-- | @join@ is used to join Matrix rooms.
--
-- = Arguments
--
-- The first argument is a representation of the Matrix room which
-- Matel's user should join.
--
-- If the room which is joined is public, then the second argument
-- should probably be 'Nothing'.  If the room which should be joined
-- is private _and_ Matel's user receives an invitation to this Matrix
-- room, then the second argument should Just' be a 3-tuple of
-- a description of the user which sends the invitation to Matel's user,
-- the state key of the invitation, and the signature of the invitation.
--
-- As usual, he third argument is the authorisation information of
-- Matel's user.
--
-- = Output
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
     -> IO (Maybe ErrorCode);
join r i a = responseToMaybe <$> TP.req TP.POST [] querr joinReq a
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

-- | @kick@ is used to non-permanently remove users from Matrix rooms.
--
-- = Arguments
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
-- = Output
--
-- If an error is encountered, then a description of this error is
-- 'Just' returned.  Otherwise, an IO-monadic 'Nothing' is output.
kick :: User
     -- ^ A description of the user which should be "kicked"
     -> Room
     -- ^ The room from which the user should be removed
     -> String
     -- ^ The reason for the removal of the user
     -> Auth
     -- ^ The authorisation information
     -> IO (Maybe ErrorCode);
kick tarjay rome m = responseToMaybe <.> TP.req TP.POST [] querr kickRq
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

-- | @ban@ is used to "permanently" remove Matrix users from Matrix
-- rooms.
--
-- = Arguments
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
-- = Output
--
-- If some error, e.g., "hey, lard-ass, you can't ban the admin if you
-- yourself are banned", is encountered, then a description of this
-- error is 'Just' output.  Otherwise, 'Nothing' is returned.
ban :: User
    -- ^ A description of the user which should be banned
    -> Room
    -- ^ The room from which the user should be banned
    -> String
    -- ^ The reason for the banning of the user
    -> Auth
    -- ^ The authorisation information
    -> IO (Maybe ErrorCode);
ban tarjay rome m = responseToMaybe <.> TP.req TP.POST [] querr banReq
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

-- | @unban@ is used to reverse users' being @ban@ned.
--
-- = Arguments
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
-- = Output
--
-- If some error, e.g., "yo, tubby, you can't un-ban yourself", is
-- encountered, then a description of this error is 'Just' output.
-- Otherwise, 'Nothing' is returned.
unban :: User
      -- ^ A description of the user which should be banned
      -> Room
      -- ^ The room from which the user should be banned
      -> Auth
      -- ^ The authorisation information
      -> IO (Maybe ErrorCode);
unban tarjay rome = responseToMaybe <.> TP.req TP.POST [] querr unbanRq
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId rome ++ "/unban"
  --
  unbanRq :: BSL.ByteString
  unbanRq = fromString $
    "{\n\t" ++
      "\"user_id\": " ++ show (username tarjay) ++ "\n" ++
    "}";

-- | @leave@ is used to leave Matrix rooms.
--
-- = Arguments
--
-- The first argument specifies the room which the user leaves.  Only
-- the @roomId@ value must be defined.
--
-- The second argument is the authorisation information which is used to
-- actually leave the room.
--
-- = Output
--
-- If an error is encountered, then an 'ErrorCode' which describes
-- this error is 'Just' output.  If no error is encountered, then
-- 'Nothing' is returned.
leave :: Room
      -- ^ The room which should be left
      -> Auth
      -- ^ The authorisation information
      -> IO (Maybe ErrorCode);
leave lamersPalace = responseToMaybe <.> TP.req TP.POST [] querr ""
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId lamersPalace ++ "/leave";

-- $digitalDisplay
--
-- This section of this module contains functions which do things which
-- are primarily display-related, as opposed to being totally
-- utilitarian.

-- | @getDisplayName@ implements the Matrix API's
-- "@GET \/_matrix\/client\/r0\/profile\/{userId}\/displayname@"
-- command.
--
-- = Arguments
--
-- == First Argument
--
-- The first argument describes the user whose display name should be
-- fetched.  Only the @username@ field is used.
--
-- == Second Argument
--
-- The second argument describes the user of Matel.  This value is used
-- to determine the FQDN of the server which should be queried.  Because
-- no actual authorisation information is used, only the @homeserver@
-- value must be specified.
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
               -- ^ The user whose display name is fetched
               -> Auth
               -- ^ The authorisation information of Matel's user, used
               -- to determine the server which should be contacted
               -> IO (Either ErrorCode User);
getDisplayName u = processResponse <.> TP.req TP.GET [] querr ""
  where
  querr :: String
  querr = "/_matrix/client/r0/profile/" ++ username u ++ "/displayname"
  --
  toDispName :: Response BS.ByteString -> Stringth
  toDispName = dnr_displayname . fromJust . A.decode .
               BSL.fromStrict . getResponseBody
  --
  processResponse :: Response BS.ByteString -> Either ErrorCode User
  processResponse r = case getResponseStatusCode r of
    200 -> Right Def.user {displayname = toDispName r}
    404 -> Right Def.user {displayname = T.pack $ username u}
    -- This "404" thing accounts for users whose display names are
    -- undefined.
    _   -> Left $ responseToStringth r;
    -- This case accounts for all situations which SHOULD NOT occur,
    -- e.g., "this user does not exist" and "yo, the server done
    -- broke".  Such responses should raise "red flags"; something has
    -- gone wrong within this module, or the program which uses this
    -- module is implemented poorly.  Alternatively, the homeserver
    -- might just be a piece of crap.

-- $createsStuff
--
-- This section of the module contains some functions which are used to
-- create or -- or just publish -- objects on Matrix.

-- | @createRoom@ is used to create new Matrix rooms.
--
-- = Arguments
--
-- The first argument is a description of the room which is to be
-- created.
--
-- If the second argument is "private", then a private room is created.
-- If the second argument is "public", then a public room is created.
-- If the second argument is of some other value, then an error is
-- probably encountered such that the desired room is not created.
--
-- The third argument is the authorisation information of Matel's user.
--
-- = Output
--
-- If all goes well, then a 'Left' 'Room' value whose @roomId@ is the ID
-- of the new room is returned.
--
-- If something 'splodes, then a 'Right' 'ErrorCode' which describes the
-- 'splosion is returned.
createRoom :: Room
           -- ^ This bit describes the room which should be created.
           -> String
           -- ^ This bit describes whether the room should be private or
           -- public.
           --
           -- This value equals "private" iff the room should be
           -- private.  This value equals "public" iff the room should
           -- be a public room.
           -> Auth
           -- ^ The information which is used to authorise the request
           -> IO (Either ErrorCode Room);
createRoom r publcty = responseToEither <.> TP.req TP.POST [] querr bod
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
  responseToEither :: Response BS.ByteString -> Either ErrorCode Room
  responseToEither resp = case getResponseStatusCode resp of
    200 -> Right Def.room {roomId = roomIdOf $ getResponseBody resp}
    _   -> Left $ responseToStringth resp
  --
  roomIdOf :: BS.ByteString -> Identifier
  roomIdOf = T.unpack . fromMaybe err . (^? A.key "room_id" . A._String)
  -- \^ @fromJust@ could be used... but when processing Nothing,
  -- @fromJust@ would provide a relatively nondescriptive error message,
  -- and VARIK finds that nondescriptive error messages are crap.  As
  -- such, VARIK elects to use @fromMaybe err@ in favour of @fromJust@.
  --
  err :: Stringth
  err = error "An unexpected error occurs!  The response code \
        \indicates success... but the body of the response lacks a \
        \\"room_id\" field.\nThe homeserver could have broken \
        \spectacularly, or createRoom could contain an error.";

-- | @upload@ is used to upload files to the homeserver of Matel's user.
--
-- = Arguments
--
-- The first argument is the content of the file which is to be
-- uploaded.
--
-- The second argument is the name of the file which is to be uploaded.
-- This argument can be the current name of the file or just the
-- desired name of the file.
--
-- The third argument is -- as the reader hopefully guessed -- the
-- authorisation garbage which is used to actually upload the file.
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
-- meant to read binary files -- 'T.Text' contains _text_, as opposed to
-- _strings of bytes_; read the name, foo' -- rather, this problem is
-- the fault of the author of @upload@.
--
-- PROTIP: Using the most fitting tools prevents a decent number of
-- problems.
upload :: BSL.ByteString
       -- ^ The content of the file which should be uploaded
       -> String
       -- ^ The name of the file which should be uploaded
       -> Auth
       -- ^ The authorisation information
       -> IO (Either ErrorCode Stringth);
upload attachment name = process <.> TP.req TP.POST hdr qq atch
  where
  process :: Response BS.ByteString -> Either ErrorCode Stringth
  process k = case getResponseStatusCode k of
    200 -> Right $ fromJust $
             (Q..! "{content_uri}") <$>
             Q.decode (BSL.fromStrict $ getResponseBody k)
    _   -> responseToLeftRight k
  --
  hdr :: [(HeaderName, BS.ByteString)]
  hdr = [("Content-Type", "text/plain")]
  --
  atch :: BSL.ByteString
  atch = attachment
  --
  qq :: String
  qq = "_matrix/media/r0/upload?filename=" ++
       toString (urlEncode True $ fromString name);

-- $spam
--
-- This section of the module contains the functions and whatnot which
-- facilitate sending stuff to Matrix rooms.

-- $cryptoShit
--
-- This section of the module contains functions which directly
-- interface with Matrix's cryptographic protocols.

-- $genericDescribe
--
-- This section of the module contains functions which describe things
-- such that these descriptions can be used for utilitarian purposes, as
-- opposed to being purely display-related.

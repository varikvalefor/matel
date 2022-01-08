{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Main
-- Description : Business end of MATELCLI
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
-- @matelcli@ is a command-line interface for Matrix which uses
-- Matel's underlying "Metal" infrastructure.
--
-- @matelcli@ is written such that for all text-based features of Matel
-- @k@, @matelcli@ is capable of doing @k@.
--
-- The user-facing documentation/specification of @matelcli@ is
-- available in @matelcli@'s manual page, which is by default located at
-- @[MATEL GIT REPOSITORY DIRECTORY]\/matelcli.1@.  This documentation
-- is only of particular interest to men who wish to modify @matelcli@
-- or understand the inner workings of @matelcli@.
module Main where
import Plegg;
import GetAuth;
import Text.Read;
import Data.Maybe;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.Auth;
import Data.Either;
import System.Exit;
import Metal.Space;
import Metal.Community;
import Control.Concurrent;
import System.Environment;
import Control.Monad ((>=>));
import Metal.Messages.FileInfo;
import Metal.Messages.Standard;
import Metal.EventCommonFields;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Text.IO as T;
import Metal.MatrixAPI.HighLevel as H;
import qualified Metal.Default as Def;
import qualified Data.ByteString.Lazy as BSL;

-- | Chicken chow mein main...
main :: IO ();
main =
  univac >> plegg >>
  getAuthorisationDetails >>= \aufFile ->
    getArgs >>= flip determineAction aufFile;

-- | @determineAction@ is used to determine the action which should be
-- taken by @matelcli@, e.g., listing stuff or sending a message.
determineAction :: [String]
                -- ^ The input @matelcli@ command
                -> Auth
                -- ^ Matel user's authorisation information
                -> IO ();
determineAction [] a = error "I never thought that I would have a \
  \stress-induced heart attack by the age of forty, but you're making \
  \me rethink some things.";
determineAction (command:stuff) a = case command of
  "list"       -> list stuff a
  "send"       -> Main.send stuff a
  "grab"       -> grab stuff a
  "login"      -> logIn a
  "markread"   -> mkRead stuff a
  "sync"       -> eddySmith stuff a >>= T.putStrLn
  "join"       -> runJoin stuff a
  "leave"      -> runLeave stuff a
  "kick"       -> runKick stuff a
  "createroom" -> createRoom' stuff a
  "upload"     -> ooplawed stuff a
  _            -> error "An unrecognised command is input.  \
                  \RTFM, punk.";

-- | The "list" command is used to list stuff, e.g., rooms of which the
-- user is a member.
--
-- @list ["rooms"] a@ lists the Matrix rooms of which the user who is
-- specified in @a@ is a member.
--
-- @list ["communities"] a@ lists the Matrix communities of which the
-- user who is specified in @a@ is a member.
--
-- @list ["spaces"] a@ lists the Matrix spaces of which the user who is
-- specified in @a@ is a member.
list :: [String] -> Auth -> IO ();
list [] = error "You wimps suck.";
list (k:_) = case k of
  "rooms"       -> memberRooms >=> mapM_ (putStrLn . roomId)
  "communities" -> memberComms >=> mapM_ (putStrLn . commId)
  "spaces"      -> memberSpaces >=> mapM_ (putStrLn . spaceId)
  _             -> error "The police will be listing your injuries \
                   \if you don't stop inputting crap.";

-- | @send@ implements the "send" command.
--
-- Via the account which is described in @n@,
-- @send ["text", foo] n@ sends a message whose body is the standard
-- input to the chatroom whose internal Matrix ID is @foo@.
--
-- Via the account which is described in @n@,
-- @send ["file", k, foo] n@ sends a message whose attachment is
-- @k@ to the chatroom whose internal Matrix ID is @foo@.
send :: [String]
     -- ^ The ['String']-based command-line arguments, e.g.,
     -- @["text", "!hskonBonfjiIefqLUV:matrix.org"]@
     -> Auth
     -- The information which is used to authenticate Matel's user
     -> IO ();
send k a
  | k == [] = error "I need some arguments, fat-ass."
  | length k < 2 = error "I thought that you were improving.  I now \
                   \see that I was wrong.  Really, I should be mad at \
                   \myself for apparently going insane by having some \
                   \faith in you."
  | otherwise = getTarget >>= \t -> H.send t dest a >>= dispError
  where
  getTarget :: IO StdMess
  getTarget = case head k of
    "text"     -> T.getContents >>= \input ->
                  return Def.stdMess {body = input}
    "file"     -> uploadStdinGetID (k !! 1) a >>= \uploadID ->
                  return Def.stdMess {
                    msgType = Attach,
                    body = T.pack $ k !! 1,
                    url = Just $ T.unpack uploadID,
                    fileInfo = Just Def.fileInfo {
                      mimetype = Just "text/plain"
                    }
                  }
    "notice"   -> T.getContents >>= \input ->
                  return Def.stdMess {body = input, msgType = Notice}
    "location" -> T.getContents >>= \input ->
                  return Def.stdMess {
                    msgType = Location,
                    geo_uri = Just $ T.pack $ k !! 1,
                    body = input
                  }
    _          -> error "I ought to send you to the garbage disposal, \
                        \ shit-tits.  Read the fucking manual."
  --
  dest :: Room
  dest = Def.room {roomId = k !! destIndex}
    where
    diargumentalStuff :: [String]
    diargumentalStuff = ["file", "location"]
    --
    destIndex :: Int
    destIndex | head k `elem` diargumentalStuff = 2
              | otherwise = 1;
              -- \^ This bit is necessary because the number of
              -- arguments of the "send file" command is not equal to
              -- the number of arguments of the "send text" and "send
              -- notice" commands.

-- | @uploadStdinGetID@ uploads some data which is read from the
-- standard input to a homeserver, returning the URI of the uploaded
-- file if everything goes according to plan.
--
-- = Arguments
--
-- The first argument is the desired filename of the uploaded data.
--
-- The second argument is the authorisation garbage of Matel's user.
--
-- = Output
--
-- If everything goes according to plan, then the MXC URI of the
-- uploaded file is returned.
--
-- However, if NOT(EVERYTHING GOES ACCORDING TO PLAN), then
-- @uploadStdinGetID@ probably just bursts into flame.
uploadStdinGetID :: String
                 -- ^ The name of the file which is uploaded
                 -> Auth
                 -- ^ The authorisation information which is used to
                 -- upload the file
                 -> IO Stringth;
uploadStdinGetID p90 = either (error . T.unpack) id <.> uploadThing
  where
  uploadThing :: Auth -> IO (Either Stringth Stringth)
  uploadThing off = BSL.getContents >>= \c -> upload c p90 off;

-- | @grab@ is used to fetch and output the messages of a room.
--
-- = Arguments
--
-- The first argument is a 4-list of the number of messages which are
-- fetched, "early" or "recent", an unused thing, and the internal
-- Matrix ID of the Matrix room from which the messages are fetched.
--
-- The second argument is the authorisation crap of Matel's user.
grab :: [String]
     -- ^ The first 3 elements of this list are the decimal number of
     -- messages which should be nabbed, "early" or "recent", some junk
     -- data, and the internal Matrix ID of the room from which messages
     -- should be nabbed.
     -> Auth
     -- ^ The authorisation information
     -> IO ();
grab (decino:eeyore:jd:mexicid:_) a
  | n < 0 = error "I need a natural number, not garbage."
  | n == 0 = error "Why in the hell would you want to take 0 messages?\
                   \  0 is not a natural number, anyway."
  | otherwise = case eeyore of
    "recent" -> recentMessagesFrom n room a >>= mapM_ print
    "early"  -> earlyMessagesFrom n room a >>= mapM_ print
    _        -> error "I'll grab you if you don't grab some sense."
  where
  -- \| This variable refers to the number of messages which should be
  -- fetched.
  n :: Integer
  n = fromMaybe (-42) $ readMaybe decino
  --
  room :: Room
  room = Def.room {roomId = mexicid};
grab _ _ = error "Repent, motherfucker.";

-- | @mkRead [identifier] a@ marks the message whose identifier is
-- @identifier@ as having been read if this message exists.  @a@ is used
-- to authorise the request.
mkRead :: [String]
       -- ^ [MESSAGE ID OF MESSAGE WHAT SHOULD DONE BE READ]
       -> Auth
       -- ^ The authorisation information which is used to mark the
       -- message as having been read.
       -> IO ();
mkRead [] = error "Someone should knock you upside the head a few \
                  \times, punk.  Dismissed.";
mkRead (eeee:_) = markRead Def.stdMess {boilerplate = boi} >=> dispError
  where
  boi :: EventCommonFields
  boi = Def.eventCommonFields {eventId = eeee};

-- | @dispError@ displays error messages without needlessly feeding
-- lines.
--
-- If @k == Nothing@, then @dispError k@ does nothing.  @dispError k@
-- otherwise runs @error k@.
dispError :: Maybe ErrorCode -> IO ();
dispError = maybe (return ()) (error . T.unpack);

-- | @logIn k@ generates an authorisation token for the user which is
-- specified in @k@ and writes this authorisation token to the standard
-- output.
logIn :: Auth -> IO ();
logIn = loginPass >=> either busticate addAndDisplay
  where
  addAndDisplay :: T.Text -> IO ();
  addAndDisplay toke = configFilePath >>= \path ->
                       T.readFile path >>= \phile ->
                       T.putStrLn toke >>
                       T.writeFile path (addToken phile toke)
  --
  addToken :: T.Text -> T.Text -> T.Text
  addToken phile toke = T.unlines $ (++ [T.append "authtoken: " toke]) $
                        filter ((/= "authtoken: ") . T.take 11) $
                        T.lines phile
  --
  busticate :: T.Text -> IO ()
  busticate = error . ("logIn: " ++) . T.unpack;

-- | @eddySmith@ is a command-line-friendly wrapper for @'sync'@.
--
-- = Arguments
--
-- The first argument determines the "since" value which is attached
-- to the "sync" request.  If the first argument is @[]@, then no
-- "since" value is attached.  If the first argument is some other
-- thing, then the first element of this argument is used as the "since"
-- value of the sync request.
--
-- The second argument, as ever, is the boring boilerplate authorisation
-- information.
--
-- = Output
--
-- The output is the verbatim body of the homeserver's response to the
-- "sync" request.
eddySmith :: [String]
          -- ^ The arguments of the @matelcli@ command
          -> Auth
          -- ^ The authorisation information of Matel's user
          -> IO Stringth;
eddySmith t = either (error . T.unpack) id <.> sync since
  where
  since :: Maybe String
  since
    | t == [] = Nothing
    | otherwise = Just $ head t;

-- | @runJoin@ is a relatively command-line-friendly wrapper for
-- @'join'@.
--
-- = Arguments
--
-- The first argument contains the command-line arguments of the @join@
-- command.  This thing should be a 1-list or a 4-list.
--
-- If the first argument is a 1-list, then this 1-list simply contains
-- the internal Matrix ID of the room which should be joined.
--
-- If the first argument is a 4-list, indicating that some user has
-- actively invited Matel's user to the Matrix room which should be
-- joined, then this 4-list contains, in order, the internal Matrix
-- ID of the room which is joined, the state key of some invitation
-- which Matel's user receives, and the signature of this invite.
--
-- The second argument is just the standard authorisation stuff.
runJoin :: [String]
        -- ^ The arguments of the @matelcli@ command, e.g.,
        -- @["!UxQFGskJBlUowxdIxQ:tapenet.org"]@
        -> Auth
        -- ^ The authorisation information of Matel's user
        -> IO ();
runJoin [] = error "Idiot!  How am I to join an unspecified room for \
                   \you?  My strength is simplicity.  I can't work \
                   \with this shit.";
runJoin t = join room inviteInfo >=> dispError
  where
  room :: Room
  room = Def.room {roomId = t !! 0}
  --
  inviteInfo :: Maybe (User, String, String)
  inviteInfo = case length t of
    4 -> Just (Def.user {username = t !! 1}, t !! 2, t !! 3)
    1 -> Nothing
    _ -> error "You have managed to completely disregard the \
         \information which is specified in my manual page by \
         \inputting a weird number of arguments, which is \
         \actually not terribly impressive... but is still a bit \
         \irritating.";

-- | @runLeave@ is a relatively high-level interface for the @'leave'@
-- command.
--
-- = Arguments
--
-- The first element of the first argument is the room ID of the room
-- which the user should leave.  This element is the only element which
-- must be contained within this argument -- this argument is only of
-- type ['String'] and not 'String' because just making this thing use
-- a ['String'] facilitates writing a clean @'determineAction'@.
--
-- The second argument is the authorisation information which is used to
-- actually leave the specified room.
--
-- = Processing
--
-- An error is encountered iff an error is thrown.
runLeave :: [String]
         -- ^ [ROOM ID OF THE ROOM WHAT SHOULD BE LEFT]
         -> Auth
         -- ^ Matel user's authorisation information
         -> IO ();
runLeave (x:_) = leave Def.room {roomId = x} >=> dispError;
runLeave _ = error "You'd best leave... or stop giving me \
                   \nothing but bullshit.";

-- | @runKick@ is a relatively command-line-friendly interface for the
-- @'kick'@ command.
--
-- = Arguments
--
-- The first argument is a 3-list of the username of the user which
-- should be kicked, the internal Matrix ID of the room from which
-- the user should be kicked, and a reason for the kicking of the user.
-- If the reason is @""@, then no reason is supplied.
--
-- The second argument is _still_ the same old authorisation stuff.
--
-- = Processing
--
-- An error is encountered iff an error is thrown.
runKick :: [String]
        -- ^ The first 3 elements of this list are the room ID of the
        -- room from which the user should be removed, and the reason
        -- for the removal of this user.  If the third element equals
        -- @""@, then no reason is supplied.
        -> Auth
        -- The information which is used to authorise the kicking of the
        -- user
        -> IO ();
runKick (uninat:cell:remo:_) = kick user room remo >=> dispError
  where
  user = Def.user {username = uninat}
  room = Def.room {roomId = cell};
runKick _ = error "I'll kick YOUR ass if you don't start giving \
                  \me some actual directions."

-- | @createRoom'@ is used to create new Matrix rooms.
--
-- = Arguments
--
-- The first argument is a 3-list whose elements, in order, are the
-- display name of the Matrix room which should be created, the topic
-- message of the Matrix room which should be created, and "private" or
-- "public", depending upon whether the new Matrix room should be
-- private or public.
--
-- The second and final argument is STILL authorisation crap.
--
-- = Processing
--
-- If the creation of this room is a success, then the internal Matrix
-- ID of this Matrix room is written to the standard output.  If
-- something violently falls apart, then an error is thrown.
createRoom' :: [String]
            -- ^ The command-line arguments
            -> Auth
            -- ^ The information which is used to authorise the request
            -> IO ();
createRoom' [] = error "";
-- The above error message is a particularly smart-ass error message.
--
-- The error message in question might not be particularly enlightening,
-- but reading the manual page should yield the desired enlightenment.
createRoom' [_] = error "Your one-word demands are starting to piss me \
                  \off.";
createRoom' [_,_] = error "Should I just assume that you want to make \
                    \all of your communications public?";
createRoom' (nm:tpc:pbl:_) = createRoom rm pbl >=> display
  where
  rm :: Room
  rm = Def.room {roomName = Just $ T.pack nm, topic = Just $ T.pack tpc}
  --
  display :: Either ErrorCode Room -> IO ()
  display = either (error . T.unpack) (putStrLn . roomId);

-- | @messToHumanReadable k@ is roughly equivalent to @show k@.
-- However, VARIK finds that the readability of @messToHumanReadable k@
-- is greater than the readability of @show k@.
messToHumanReadable :: StdMess
                    -- ^ The message which should be described
                    -> String;
messToHumanReadable k =
  "At " ++ show (origin_server_ts $ boilerplate k) ++ ", " ++
  username (sender $ boilerplate k) ++ " sends the following " ++
  show (msgType k) ++ ": " ++ show (body k);

-- | @ooplawed@ uploads a file which is read from the standard input to
-- the homeserver of Matel's user.
--
-- = Arguments
--
-- The first argument is the @'tail'@ of Matel's command-line arguments.
-- The first element of this list is the desired name of the uploaded
-- file.
--
-- The second argument is the authorisation information which is used to
-- upload the file to the Matrix homeserver.
--
-- = Uploading Unencrypted Files
--
-- Users of @ooplawed@ should note that @ooplawed@ uploads UNENCRYPTED
-- files.  When the @protocol@ is HTTPS, @ooplawed@ _is_ TLS-protected.
-- However, @ooplawed@ does _not_ support "true" end-to-end encryption.
ooplawed :: [String]
         -- ^ The @'tail'@ of the command-line arguments
         -> Auth
         -- ^ The authorisation information
         -> IO ();
ooplawed (f:_) = uploadStdinGetID f >=> T.putStrLn;

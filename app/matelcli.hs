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
-- The user-facing documentation/specification of the user interface is
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
list [] _ = error "Write an argument or be made sufficiently \
            \brain-dead to never write arguments again.";
list (k:_) a = case k of
  "rooms"       -> memberRooms a >>= mapM_ (putStrLn . roomId)
  "communities" -> memberComms a >>= mapM_ (putStrLn . commId)
  "spaces"      -> memberSpaces a >>= mapM_ (putStrLn . spaceId)
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
  | otherwise = target >>= \t -> H.send t dest a >>= dispError
  where
  target :: IO StdMess
  target = case head k of
    "text"     -> T.getContents >>= \input ->
                  return Def.stdMess {body = input}
    "file"     -> BSL.getContents >>= \content ->
                  upload content (k !! 1) a >>=
                  return . processError >>= \uploadID ->
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
              -- This bit is necessary because the number of arguments of the
              -- "send file" command is not equal to the number of arguments
              -- of the "send text" and "send notice" commands.

-- | @grab@ is used to fetch and output the messages of a room.
--
-- @grab@'s argument follows the pattern [NUMBER OF MESSAGES, "EARLY" OR
-- "RECENT", JUNK DATA, ID OF DESIRED MATRIX ROOM].
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
dispError = maybe (return ()) error;

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
-- If @t == []@, then @eddySmith t a@ sends a "since"-less "sync"
-- query to the Matrix homeserver.  @eddySmith t a@ otherwise sends a
-- "sync" query whose "since" value equals @t !! 1@.
eddySmith :: [String]
          -- ^ The @matelcli@ command, e.g., "@sync bullshit@"
          -> Auth
          -- ^ The authorisation information of Matel's user
          -> IO Stringth;
eddySmith t a = either (error . T.unpack) id <$> sync since a
  where
  since :: Maybe String
  since
    | t == [] = Nothing
    | otherwise = Just $ head t;

-- | @runJoin@ is a relatively command-line-friendly wrapper for 'join'.
runJoin :: [String]
        -- ^ The arguments of the @matelcli@ command, e.g.,
        -- @["!UxQFGskJBlUowxdIxQ:tapenet.org"]@
        -> Auth
        -- ^ The authorisation information of Matel's user
        -> IO ();
runJoin [] a = error "Idiot!  How am I to join an unspecified room for \
                     \you?  My strength is simplicity.  I can't work \
                     \with this shit.";
runJoin t a = join room inviteInfo a >>= dispError
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
-- The first element of the first argument is the room ID of the room
-- which the user should leave.  This element is the only element which
-- must be contained within this argument -- this argument is only of
-- type ['String'] and not 'String' because just making this thing use
-- a ['String'] facilitates writing a clean @'determineAction'@.
--
-- The second argument is the authorisation information which is used to
-- actually leave the specified room.
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
-- @runKick [user, room, reason]@ kicks user @user@ from the Matrix room
-- whose internal Matrix ID is @room@, justifying the kicking with
-- @reason@.  If @reason == []@, then no reason is supplied.
runKick :: [String]
        -- ^ The first 3 elements of this list are the room ID of the
        -- room from which the user should be removed, the reason for
        -- the removal of this user, and the reason for the removal of
        -- this user.  If the third element equals @""@, then no reason
        -- is supplied.
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

-- | @createRoom [name_, topic_, permission_] a@ should create a Matrix
-- room @k@ such that @roomName k == name_@ and @topic k == topic_@.  If
-- @permission_ == "private"@, then a private room should be created.
-- If @permission_ == "public"@, then a public room should be created.
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
  rm = Def.room {roomName = T.pack nm, topic = T.pack tpc}
  --
  display :: Either String Room -> IO ()
  display = either error (putStrLn . roomId);

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

-- | @ooplawed (filename:_) a@ uploads the file whose content is read
-- from the standard input to the homeserver of @a@.  The homeserver
-- is told that the filename of the uploaded file is @filename@.
--
-- Users of @ooplawed@ should note that @ooplawed@ uploads UNENCRYPTED
-- files.  @ooplawed@ should be TLS-protected but does not support
-- end-to-end encryption.
ooplawed :: [String]
         -- ^ The @'tail'@ of the command-line arguments
         -> Auth
         -- ^ The authorisation information
         -> IO ();
ooplawed (f:_) a = BSL.getContents >>= \c -> upload c f a >>= process
  where
  process :: Either Stringth Stringth -> IO ()
  process = either (error . T.unpack) T.putStrLn;

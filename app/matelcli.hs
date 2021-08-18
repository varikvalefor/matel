{-# LANGUAGE OverloadedStrings #-}

-- | @matelcli@ is a command-line interface for Matrix which uses
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
import Metal.Messages.Standard;
import Metal.OftenUsedFunctions;
import Metal.MatrixAPI.HighLevel;
import qualified Metal.Default as Def;
import Metal.MatrixAPI.LowLevel (loginPass, sync, join, leave, kick);

import qualified Data.Text as T;
import qualified Data.Text.IO as T;

-- | Chicken chow mein main...
main :: IO ();
main =
  getAuthorisationDetails >>= \aufFile ->
  getArgs >>= flip determineAction aufFile;

-- | @determineAction@ is used to determine the action which should be
-- taken by @matelcli@, e.g., listing stuff or sending a message.
determineAction :: [String]
                -- ^ The input @matelcli@ command
                -> Auth
                -- ^ Matel user's authorisation information
                -> IO ();
determineAction [] a = error $ "I never thought that I would have a " ++
  "stress-induced heart attack by the age of forty, but " ++
  "you're making me rethink some things.";
determineAction (command:stuff) a =
  case command of
    "list"     -> list stuff a
    "send"     -> send stuff a
    "grab"     -> grab stuff a
    "login"    -> logIn a
    "markread" -> mkRead stuff a
    "sync"     -> eddySmith stuff a >>= T.putStrLn
    "join"     -> runJoin stuff a
    "leave"    -> runLeave stuff a
    "kick"     -> runKick stuff a
    _          -> error $ "An unrecognised command is input.  " ++
                  "RTFM, punk.";

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
list [] _ = error $ "Write an argument or be made sufficiently " ++
    "brain-dead to never write arguments again.";
list (k:_) a =
  case k of
    "rooms"       -> memberRooms a >>= mapM_ (putStrLn . roomId)
    "communities" -> memberComms a >>= mapM_ (putStrLn . commId)
    "spaces"      -> memberSpaces a >>= mapM_ (putStrLn . spaceId)
    _             -> error $ "The police will be listing your " ++
                     "injuries if you don't stop inputting crap.";

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
  | length k < 2 = error $ "I thought that you were improving.  " ++
    "I now see that I was wrong.  Really, I should be mad at myself " ++
    "for apparently going insane by having some faith in you."
  | otherwise = target >>= \t -> isSentToRoom t dest a >>= dispError
  where
  target :: IO StdMess
  target =
    case head k of
    "text" -> T.getContents >>= \input ->
              return Def.stdMess {body = input}
    "file" -> error "Sending files is unimplemented."
    _      -> error $ "I ought to send you to the garbage " ++
              "disposal, shit-tits.  Read the fucking manual."
  --
  dest :: Room
  dest = Def.room {roomId = k !! n}
    where
    n :: Int
    n | head k == "file" = 2
      | otherwise = 1;
      -- This bit is necessary because the number of arguments of the
      -- "send file" command is not equals to the number of arguments
      -- of the "send text" command.

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
grab k a
  | k == [] = error "Repent, motherfucker."
  | n < 0 = error "I need a natural number, not garbage."
  | n == 0 = error "Why in the hell would you want to take 0 messages?"
  | otherwise = case k !! 1 of
    "recent" -> recentMessagesFrom n room a >>= mapM_ print
    "early"  -> earlyMessagesFrom n room a >>= mapM_ print
    _        -> error "I'll grab you if you don't grab some sense."
  where
  n :: Integer
  n = fromMaybe (-42) $ readMaybe $ head k
  --
  order :: String
  order = k !! 1
  --
  room :: Room
  room = Def.room {roomId = k !! 3};

-- | @mkRead [identifier] a@ marks the message whose identifier is
-- @identifier@ as having been read if this message exists.  @a@ is used
-- to authorise the request.
mkRead :: [String]
       -- ^ [MESSAGE ID OF MESSAGE WHAT SHOULD DONE BE READ]
       -> Auth
       -- ^ The authorisation information which is used to mark the
       -- message as having been read.
       -> IO ();
mkRead [] a = error $ "Someone should knock you upside the head a " ++
  "few times, punk.  Dismissed.";
mkRead k a = markRead Def.stdMess {messageId = head k} a >>= dispError;

-- | @dispError@ is used to display error messages without needlessly
-- feeding lines.
--
-- If @k == Nothing@, then @dispError k@ does nothing.  @dispError k@
-- otherwise runs @error k@.
dispError :: Maybe ErrorCode -> IO ();
dispError = maybe (return ()) error;

-- | @logIn k@ generates an authorisation token for the user which is
-- specified in @k@.
logIn :: Auth -> IO ();
logIn = loginPass >=> either busticate T.putStrLn
  where
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
runJoin [] a = error $ "Idiot!  How am I to join an unspecified " ++
  "room for you?  My strength is simplicity.  I can't work with " ++
  "this shit."
runJoin t a = join room inviteInfo a >>= dispError
  where
  room :: Room
  room = Def.room {roomId = t !! 0}
  --
  inviteInfo :: Maybe (User, String, String)
  inviteInfo =
    case length t of
      4 -> Just (Def.user {username = t !! 1}, t !! 2, t !! 3)
      1 -> Nothing
      _ -> error $ "You have managed to completely disregard the " ++
           "information which is specified in my manual page by " ++
           "inputting a weird number of arguments, which is " ++
           "actually not terribly impressive... but is still a bit " ++
           "irritating.";

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
runLeave [] _ = error $ "You'd best leave... or stop giving me " ++
  "nothing but bullshit.";
runLeave (x:_) a = leave Def.room {roomId = x} a >>= dispError;

-- | @runKick@ is a relatively command-line-friendly interface for the
-- @'kick'@ command.
--
-- @runKick [user, room, reason]@ kicks user @user@ from the Matrix room
-- @room@, justifying the kicking with @reason@.
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
runKick k a
  | length k < 3 = error $ "I'll kick YOUR ass if you don't start " ++
    "giving me some actual directions."
  | otherwise = kick user room (k !! 2) a >>= dispError
  where
  user :: User
  user = Def.user {username = k !! 0}
  --
  room :: Room
  room = Def.room {roomId = k !! 1};

{-# LANGUAGE OverloadedStrings #-}

-- | @matelcli@ is a command-line interface for Matrix which uses
-- Matel's underlying "Metal" infrastructure.
--
-- The user-facing documentation/specification of the user interface is
-- available in @matelcli@'s manual page, which is by default located at
-- @[MATEL GIT REPOSITORY DIRECTORY]\/matelcli.1@.  This documentation
-- is only of particular interest to men who wish to modify @matelcli@
-- or understand the inner workings of @matelcli@.
module Main where
import GetAuth;
import Text.Read;
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
  getArgs >>= \command ->
  getAuthorisationDetails >>= \aufFile ->
  determineAction command aufFile;

-- | @determineAction@ is used to determine the action which should be
-- taken by @matelcli@, e.g., listing stuff or sending a message.
determineAction :: [String]
                -- ^ The input @matelcli@ command
                -> Auth
                -- ^ Matel user's authorisation information
                -> IO ();
determineAction x a
  | x == [] = error $ "I never thought that I would have a " ++
    "stress-induced heart attack by the age of forty, but " ++
    "you're making me rethink some things."
  | com == "list" = list stuff a
  | com == "send" = send stuff a
  | com == "grab" = grab stuff a
  | com == "login" = logIn a
  | com == "markread" = mkRead stuff a
  | com == "sync" = eddySmith stuff a >>= T.putStrLn
  | com == "join" = runJoin stuff a
  | com == "leave" = runLeave stuff a
  | com == "kick" = runKick stuff a
  | otherwise = error $ "An unrecognised command is input.  " ++
    "RTFM, punk."
  where
  com :: String
  com = head x
  --
  stuff :: [String]
  stuff = tail x;

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
list k a
  | k == [] = error $ "Write an argument or be made sufficiently " ++
    "brain-dead to never write arguments again."
  | is "rooms" = memberRooms a >>= mapM_ (putStrLn . roomId)
  | is "communities" = memberComms a >>= mapM_ (putStrLn . commId)
  | is "spaces" = memberSpaces a >>= mapM_ (putStrLn . spaceId)
  | otherwise = error $ "The police will be listing your injuries " ++
    "if you don't stop inputting crap."
  where
  is :: String -> Bool
  is = (head k ==);

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
     -- ^ The ['String']-based @matelcli@ command, e.g., @["send",
     -- "text", "!hskonBonfjiIefqLUV:matrix.org"]@
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
  target
    | typeIs "text" =
      T.getContents >>= \input -> return Def.stdMess {body = input}
    | typeIs "file" = error "Sending files is unimplemented."
    | otherwise = error $ "I ought to send you to the garbage " ++
      "disposal, shit-tits.  Read the fucking manual."
  --
  dest :: Room
  dest = Def.room {roomId = k !! n}
    where
    n :: Int
    n | head k == "file" = 2
      | otherwise = 1
      -- This bit is necessary because the number of arguments of the
      -- "send file" command is not equals to the number of arguments
      -- of the "send text" command.
  --
  typeIs :: String -> Bool
  typeIs = (head k ==);

-- | @grab@ is used to fetch and output the messages of a room.
--
-- @grab@'s argument follows the pattern [NUMBER OF MESSAGES, "EARLY" OR
-- "RECENT", JUNK DATA, ID OF DESIRED MATRIX ROOM].
grab :: [String] -> Auth -> IO ();
grab k a
  | k == [] = error "Repent, motherfucker."
  | n < 0 = error "I need a natural number, not garbage."
  | n == 0 = error "Why in the hell would you want to take 0 messages?"
  | order == "recent" = recentMessagesFrom n room a >>= mapM_ print
  | order == "early" = earlyMessagesFrom n room a >>= mapM_ print
  | otherwise = error "I'll grab you if you don't grab some sense."
  where
  n :: Integer
  n = maybe (-42) id $ readMaybe $ head k
  --
  order :: String
  order = k !! 1
  --
  room :: Room
  room = Def.room {roomId = k !! 3};

-- | @mkRead [identifier]@ marks the message whose identifier is
-- @identifier@ as having been read if this message exists.
mkRead :: [String] -> Auth -> IO ();
mkRead k a
  | k == [] = error $ "Someone should knock you upside the head a " ++
    "few times, punk.  Dismissed."
  | otherwise = markRead melleMel a >>= dispError
  where
  melleMel :: StdMess
  melleMel = Def.stdMess {messageId = head k};

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

-- | @eddySmith@ is a command-line-friendly wrapper for @sync@.
--
-- If @length t < 1@, then @eddySmith t a@ sends a "since"-less "sync"
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
runJoin t a
  | t == [] = error $ "Idiot!  How am I to join an unspecified " ++
    "room for you?  My strength is simplicity.  I can't work with " ++
    "this shit."
    -- The "@t == []@" case is placed here because inputting the null
    -- list would otherwise break the definition of @room@.  But moving
    -- this thing to the definition of @inviteInfo@ would certainly look
    -- relatively nice.
  | otherwise = join room inviteInfo a >>= dispError
  where
  room :: Room
  room = Def.room {roomId = t !! 0}
  --
  inviteInfo :: Maybe (User, String, String)
  inviteInfo
    | length t == 4 = Just (user, t !! 2, t !! 3)
      where user = Def.user {usermane = t !! 1}
    | length t == 1 = Nothing
    | otherwise = error $ "You have managed to completely disregard " ++
      "the information which is specified in my manual page by " ++
      "inputting a weird number of arguments, which is actually not " ++
      "terribly impressive... but is still a bit irritating.";

-- | @runLeave@ is a relatively high-level interface for the @'leave'@
-- command.
--
-- The first element of the first argument is the room ID of the room
-- which the user should leave.
runLeave :: [String]
         -- ^ [ROOM ID OF THE ROOM WHAT SHOULD BE LEFT]
         -> Auth
         -- ^ Matel user's authorisation information
         -> IO ();
runLeave g a
  | g == [] = error $ "You'd best leave... or stop giving me " ++
    "nothing but bullshit."
  | otherwise = leave Def.room {roomId = head g} a >>= dispError;

-- | @runKick@ is a relatively command-line-friendly interface for the
-- @'kick'@ command.
--
-- @runKick [user, room, reason]@ kicks user @user@ from the Matrix room
-- @room@, justifying the kicking with @reason@.
runKick :: [String] -> Auth -> IO ();
runKick k a
  | length k < 3 = error $ "I'll kick YOUR ass if you don't start " ++
    "giving me some actual directions."
  | otherwise = kick user room reason a >>= dispError
  where
  user :: User
  user = Def.user {username = k !! 0}
  --
  room :: Room
  room = Def.room {roomId = k !! 1}
  --
  reason :: String
  reason = k !! 2;

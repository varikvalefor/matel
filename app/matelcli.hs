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
import Metal.Messages.Standard;
import Metal.OftenUsedFunctions;
import Metal.MatrixAPI.HighLevel;
import qualified Metal.Default as Def;
import Metal.MatrixAPI.LowLevel (loginPass, sync, join, leave, kick);

import qualified Data.Text as T;
import qualified Data.Text.IO as T;

main :: IO ();
main =
  getArgs >>= \command ->
  getAuthorisationDetails >>= \aufFile ->
  determineAction command aufFile;

-- | @determineAction@ is used to determine the action which should be
-- taken by @matelcli@, e.g., listing stuff or sending a message.
determineAction :: [String] -- ^ The input @matelcli@ command
                -> Auth -- ^ Matel user's authorisation information
                -> IO ();
determineAction x a
  | x == [] = error "I need a command, jack-ass."
  | com == "list" = list stuff a
  | com == "send" = send stuff a
  | com == "grab" = grab stuff a
  | com == "login" = logIn a
  | com == "markread" = mkRead stuff a
  | com == "sync" = eddySmith x a >>= T.putStrLn
  | com == "join" = runJoin stuff a
  | com == "leave" = runLeave stuff a
  | com == "kick" = runKick stuff a
  | otherwise = error $ "An unrecognised command is input.  " ++
    "RTFM, punk."
  where
  com :: String
  com = x !! 0
  --
  stuff :: [String]
  stuff = tail x;

-- | The "list" command is used to list stuff, e.g., rooms of which the
-- user is a member.
--
-- A list of the accepted arguments of the "list" command is visible in
-- the function definition of @list@.
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
-- @send ["text", k, _, foo] n@ sends a message whose body is @k@ to the
-- chatroom whose internal Matrix ID is @foo@.
--
-- Via the account which is described in @n@, @send ["file", k, _, foo]@
-- sends a message whose attachment is @k@ to the chatroom whose
-- internal Matrix ID is @foo@.
send :: [String] -> Auth -> IO ();
send k a
  | k == [] = error "I need some arguments, fat-ass."
  | typeIs "text" = isSentToRoom target dest a >>= dispError
  | typeIs "file" = isSentToRoom (error "Sending files is unimplemented.") dest a >>= dispError
  | otherwise = error $ "I ought to send you to the garbage " ++
    "disposal, shit-tits.  Read the fucking manual."
  where
  target :: StdMess
  target = Def.stdMess {body = read $ k !! 1};
  --
  dest :: Room
  dest = Def.room {roomId = k !! 3}
  --
  typeIs :: String -> Bool
  typeIs = (head k ==);

-- | @grab@ is used to fetch and output the messages of a room.
--
-- @grab@'s argument follows the pattern [NUMBER OF MESSAGES, "EARLY" OR
-- "RECENT", JUNK DATA, ID OF DESIRED MATRIX ROOM].
grab :: [String] -> Auth -> IO ();
grab k a
  | n < 0 = error "I need a natural number, not garbage."
  | n == 0 = error "Why in the hell would you want to take 0 messages?"
  | order == "recent" = recentMessagesFrom n desRoom a >>= print
  | order == "early" = earlyMessagesFrom n desRoom a >>= print
  | otherwise = error "No, really, stop inputting garbage."
  where
  n :: Integer
  n = maybe (-42) id $ readMaybe $ head k
  --
  order :: String
  order = k !! 1
  --
  desRoom :: Room
  desRoom = Def.room {roomId = k !! 3};

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
logIn a = loginPass a >>= \result ->
  if isLeft result
    then error $ "loginPass: " ++ T.unpack (justLeft result)
    else T.putStrLn $ fromRight "" result;

-- | @eddySmith@ is a high-level wrapper for @sync@.
--
-- If @length t < 1@, then @eddySmith t a@ sends a "since"-less "sync"
-- query to the Matrix homeserver.  @eddySmith t a@ otherwise sends a
-- "sync" query whose "since" value equals @t !! 1@.
eddySmith :: [String] -> Auth -> IO Stringth;
eddySmith t a
  | length t > 1 = sync (Just $ t !! 1) a >>= possiblyBreakDown
  | otherwise = sync Nothing a >>= possiblyBreakDown
  where
  possiblyBreakDown :: Either Stringth Stringth -> IO Stringth
  possiblyBreakDown = either (error . T.unpack) return;

-- | @runJoin@ is a relatively high-level interface for the Matrix API
-- "join" command.
runJoin :: [String] -> Auth -> IO ();
runJoin t a
  | length t == 0 = error $ "Idiot!  How am I to join an " ++
    "unspecified room for you?  My strength is simplicity.  I can't " ++
    "work with this shit."
  | otherwise = join room inviteInfo a >>= dispError
  where
  room :: Room
  room = Def.room {roomId = t !! 0}
  --
  inviteInfo :: Maybe (User, String, String)
  inviteInfo
    | length t == 4 = Just (Def.user {username = t !! 1}, t !! 2, t !! 3)
    | not (length t `elem` [1,4]) = error $ "You have managed to " ++
      "completely disregard the information which is specified in " ++
      "my manual page by inputting a weird number of arguments, " ++
      "which is actually not terribly impressive... but is still a " ++
      "bit irritating."
    | otherwise = Nothing;

-- | @runLeave@ is a relatively high-level interface for the @'leave'@
-- command.
--
-- The first element of the first argument is the room ID of the room
-- which the user should leave.
runLeave :: [String] -> Auth -> IO ();
runLeave g a
  | g == [] = error $ "I seriously doubt that you want to leave all " ++
    "Matrix rooms or something."
  | otherwise = leave Def.room {roomId = head g} a >>=
    dispError

-- | @runKick@ is a relatively high-level interface for the @'kick'@
-- command.
--
-- @runKick [user, room, reason]@ kicks user @user@ from the Matrix room
-- @room@, justifying the kicking with @reason@.
runKick :: [String] -> Auth -> IO ();
runKick k a
  | length k < 3 = error $ "I'll kick YOUR ass if you don't start " ++
    "giving me some actual directions."
  | otherwise = kick Def.user {username = k !! 0}
                     Def.room {roomId = k !! 1}
                     (k !! 2)
                     a >>=
                dispError

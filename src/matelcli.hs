{-# LANGUAGE OverloadedStrings #-}

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
import Metal.MatrixAPI.HighLevel;
import Metal.MatrixAPI.LowLevel (loginPass);

main :: IO ();
main =
  getArgs >>= \command ->
  getAuthorisationDetails >>= \auth ->
  determineAction command auth;

-- | @determineAction@ is used to determine the action which should be
-- taken by matelcli, e.g., listing stuff or sending a message.
determineAction :: [String] -- ^ The input matelcli command
                -> Auth -- ^ Matel user's authorisation information
                -> IO ();
determineAction x a
  | x == [] = error "I need a command, jack-ass."
  | com == "list" = list stuff a
  | com == "send" = send stuff a
  | com == "grab" = grab stuff a
  | com == "login" = logIn a
  | com == "markread" = mkRead stuff a
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
  | k == [] = error "Come on.  Give me a line."
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
-- @send ["text", k, _, foo] n@ sendsa message whose body is @k@ to the
-- chatroom whose internal Matrix ID is @foo@.
--
-- Via the account which is described in @n@, @send ["file", k, _, foo]@
-- sends a message whose attachment is @k@ to the chatroom whose
-- internal Matrix ID is @foo@.
send :: [String] -> Auth -> IO ();
send k a
  | k == [] = error "I need some arguments, fat-ass."
  | typeIs "text" = isSentToRoom target dest a >>= dispError
  | typeIs "file" = error $ "Sending files is currently " ++
    "unimplemented."
  | otherwise = error $ "I ought to send you to the garbage " ++
    "disposal, punk.  Read the fucking manual."
  where
  target :: MessageText
  target = read $ k !! 1
  --
  dest :: Identifier
  dest = k !! 3
  --
  typeIs :: String -> Bool
  typeIs = (head k ==);

-- | @grab@ is used to fetch and output the messages of a room.
-- @grab@'s argument follows the pattern [NUMBER OF MESSAGES, "EARLY" OR
-- "RECENT", JUNK DATA, ID OF DESIRED MATRIX ROOM].
grab :: [String] -> Auth -> IO ();
grab k a
  | n < 0 = error "I need a natural number, not garbage."
  | n == 0 = error "Why in the hell would you want to take 0 messages?"
  | order == "recent" = error $ "Fetching the most recent messages " ++
    "is currently unimplemented."
  | order == "early" = error $ "Fetching the earliest messages is " ++
    "currently unimplemented."
  | otherwise = error "No, really, stop inputting garbage."
  where
  n :: Integer
  n = maybe (-42) (\a -> a) $ readMaybe $ head k
  --
  order :: String
  order = k !! 1
  --
  roomId :: Identifier
  roomId = k !! 3;

-- | @mkRead [identifer]@ marks the message whose identifier is
-- @identifier@ as having been read if this message exists.
mkRead :: [String] -> Auth -> IO ();
mkRead k a
  | k == [] = error $ "Someone should knock you upside the head a " ++
    "few times, punk.  Dismissed."
  | otherwise = markRead melleMel a >>= dispError
  where
  identifier :: Identifier
  identifier = head k
  melleMel :: StdMess
  melleMel = StdMess {messageId = identifier};

-- | @dispError@ is used to display error messages without needlessly
-- feeding lines.
dispError :: String -> IO ();
dispError x
  | x == "" = return ()
  | otherwise = putStrLn x >> exitFailure;

logIn :: Auth -> IO ();
logIn a = loginPass User {username = username a, password = password a} >>= \result ->
  if isLeft result
    then error $ "loginPass: " ++ fromLeft "" result
    else putStrLn $ fromRight "" result;

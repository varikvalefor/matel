{-# LANGUAGE OverloadedStrings #-}

import Text.Read;
import Metal.Base;
import Metal.Room;
import Metal.Space;
import Metal.Community;
import Control.Concurrent;
import System.Environment;
import Metal.MatrixAPI.HighLevel;

main :: IO ();
main = getArgs >>= determineAction;

-- | @determineAction@ is used to determine the action which should be
-- taken by matelcli, e.g., listing stuff or sending a message.
determineAction :: [String] -> IO ()
determineAction x
  | x == [] = error "I need a command, jack-ass."
  | com == "list" = list stuff
  | com == "send" = send stuff
  | com == "grab" = grab stuff
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
list :: [String] -> IO ();
list k
  | k == [] = error "Come on.  Give me a line."
  | is "rooms" = memberRooms >>= mapM_ (putStrLn . roomId)
  | is "communities" = memberComms >>= mapM_ (putStrLn . commId)
  | is "spaces" = memberSpaces >>= mapM_ (putStrLn . spaceId)
  | otherwise = error $ "The police will be listing your injuries " ++
    "if you don't stop inputting crap."
  where
  is :: String -> Bool
  is = (head k ==);

-- | @send@ implements the "send" command.
--
-- @send ["text", k, _, foo]@ sends a message whose body is @k@ to
-- the chatroom whose internal Matrix ID is @foo@.
--
-- @send ["file", k, _, foo]@ sends a message whose attachment is @k@
-- to the chatroom whose internal Matrix ID is @foo@.
send :: [String] -> IO ();
send k
  | k == [] = error "I need some arguments, fat-ass."
  | typeIs "text" = target `isSentToRoom` dest >>= dispError
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
grab :: [String] -> IO ();
grab k
  | pn == Nothing = error "I need a number, not garbage."
  | order == "recent" = error $ "Fetching the most recent messages " ++
    "is currently unimplemented."
  | order == "early" = error $ "Fetching the earliest messages is " ++
    "currently unimplemented."
  | otherwise = error "No, really, stop inputting garbage."
  where
  pn :: Maybe Integer
  pn = readMaybe $ k !! 0
  --
  n :: Integer
  n = read $ k !! 0
  --
  order :: String
  order = k !! 1
  --
  roomId :: Identifier
  roomId = k !! 3;

-- | @dispError@ is used to display error messages without needlessly
-- feeding lines.
dispError :: String -> IO ();
dispError x
  | x == "" = return ()
  | otherwise = putStrLn x;

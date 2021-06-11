import Metal.Base;
import Control.Concurrent;
import System.Environment;

main :: IO ();
main = getArgs >>= determineAction;

-- | determineAction is used to determine the action which should be
-- taken by matelcli, e.g., listing stuff or sending a message.
determineAction :: [String] -> IO ()
determineAction x
  | x == [] = error "I need a command, jack-ass."
  | com == "list" = list $ tail x
  | com == "send" = send $ tail x
  | otherwise = error $ "An unrecognised command is input.  " ++
    "RTFM, punk."
  where
  com = x !! 0;


-- | The "list" command is used to list stuff, e.g., rooms of which the
-- user is a member.
-- A list of the accepted arguments is visible in the function
-- definition of list.
list :: [String] -> IO ();
list k
  | k == [] = error "Come on.  Give me a line."
  | l == "rooms" = error "Listing rooms is currently unimplemented."
  | l == "communities" = error $ "Listing communities is currently " ++
    "unimplemented."
  | l == "spaces" = error "Listing spaces is currently unimplemented."
  | otherwise = error $ "The police will be listing your injuries " ++
    "if you don't stop inputting crap."
  where l = k !! 0;

-- | the "send" command is used to send messages to Matrix rooms.
-- When complete, the "send" command supports both text-based messages
-- and file-based messages.
send :: [String] -> IO ();
send k
  | k == [] = error "I need some arguments, fat-ass."
  | msgType == "text" = error $ "Sending text-based messages is " ++
    "currently unimplemented."
  | msgType == "file" = error $ "Sending files is currently " ++
    "unimplemented."
  | otherwise = error $ "I ought to send you to the garbage " ++
    "disposal, punk.  Read the fucking manual."
  where
  msgType :: String
  msgType = k !! 0
  --
  target :: String
  target = k !! 1
  --
  dest :: String
  dest = k !! 3;

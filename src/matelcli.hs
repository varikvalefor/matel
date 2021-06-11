import Metal.Base;
import Control.Concurrent;
import System.Environment;

main :: IO ();
main = getArgs >>= determineAction
  where
  determineAction :: [String] -> IO ()
  determineAction x
    | x == [] = error "I need a command, jack-ass."
    | com == "list" = list $ tail x
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

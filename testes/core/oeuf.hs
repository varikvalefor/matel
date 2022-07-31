-- | This test suite determines whether or not the functions which are
-- defined in "Metal.OftenUsedFunctions" actually work.
module Main where
import Data.Bool;
import System.Exit;
import Metal.OftenUsedFunctions;

main :: IO ExitCode;
main = bool exitFailure exitSuccess $ and correctness
  where
  correctness = [((++ "b") .: (++)) "bo" "o" == "boob",
                 ((+1) <.> (:[])) 4 == [5]];

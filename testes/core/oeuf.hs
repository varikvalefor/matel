{-# LANGUAGE OverloadedStrings #-}

-- | This test suite determines whether or not the functions which are
-- defined in "Metal.OftenUsedFunctions" actually work.
module Main where
import Data.Bool;
import System.Exit;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

main :: IO ExitCode;
main = bool exitFailure exitSuccess $ and correctness
  where
  correctness = [((++ "b") .: (++)) "bo" "o" == "boob",
                 fromString (toString hahaBS) == hahaBS,
                 fromString (toString hahaBL) == hahaBL,
                 fromString (toString hahaTX) == hahaTX,
                 ((+1) <.> (:[])) 4 == [5]];

-- | @hahaBS@ is a stupid "string" which is used to test 'toString' and
-- 'fromString'.
hahaBS :: BS.ByteString;
hahaBS = "haha";

-- | @hahaBL@ is a stupid "string" which is used to test 'toString' and
-- 'fromString'.
hahaBL :: BSL.ByteString;
hahaBL = "haha";

-- | @hahaTX@ is a stupid "string" which is used to test 'toString' and
-- 'fromString'.
hahaTX :: T.Text;
hahaTX = "haha";

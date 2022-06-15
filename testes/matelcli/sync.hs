-- | This test suite determines whether or not @matelcli@ can
-- successfully @sync@.
module Main where
import System.Exit;
import System.Process;

-- | @main@ attempts to use @matelcli@ to use the Matrix API's "@sync@"
-- command, returning the exit code which is output by @matelcli@.
main :: IO ExitCode;
main = (\(a,_,_) -> a) <$> readProcessWithExitCode "cabal" [] []
  where
  argh = ["run", "matelcli", "sync"];

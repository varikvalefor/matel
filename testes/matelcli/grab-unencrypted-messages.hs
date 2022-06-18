-- | This test suite just determines whether or not @matelcli@ can fetch
-- unencrypted messages.
module Main where
import System.Exit;
import System.Process;

-- | @main@ attempts to nab a list of some recent messages via
-- @matelcli@, returning the exit code which @matelcli@ outputs.
main :: IO ExitCode;
main = (\(a,_,_) -> a) <$> readProcessWithExitCode "cabal" argh []
  where
  argh = ["run", "matelcli", "grab", "5", "recent", roomId]
  roomId = "!tCMDotrJLICfJyyNPi:matrix.org";

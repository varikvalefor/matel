-- | This test suite just determines whether or not @matelcli@ can send
-- unencrypted messages.
module Main where
import System.Exit;
import System.Process;

-- | @main@ attempts to send a text-based message to a dedicated test
-- room via @matelcli@, returning the exit code which @matelcli@
-- outputs.
main :: IO ExitCode;
main = (\(a,_,_) -> a) <$> readProcessWithExitCode "cabal" argh mosig
  where
  argh = blah ++ [roomId]
  blah = ["run", "matelcli", "send", "text"]
  roomId = "!tCMDotrJLICfJyyNPi:matrix.org"
  mosig = "You fucked up my face.";

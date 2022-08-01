{-# LANGUAGE OverloadedStrings #-}

-- | This test suite determines whether or not Matel's cryptographic
-- stuff actually works.
module Main where
import Data.Bool;
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import System.Exit;
import Metal.MatrixAPI.LowLevel;
import qualified Metal.Default as Jam;

-- | @main@ is equivalent to @exitSuccess@ iff @main@ determines that
-- Matel's cryptographic stuff works.  @main@ is equivalent to
-- @exitFailure@ iff @main@ does not determine that Matel's
-- cryptographic stuff works.
main :: IO ExitCode;
main = bool exitFailure exitSuccess =<< and <$> sequence tests;

-- | For all @t@ @tests@, @t == 'pure' 'True'@ iff the Matel
-- cryptographic stuff which is used by @t@ actually works.
tests :: [IO Bool];
tests = [endeAlg Olm, endeAlg Megolm];

-- | For all @t@, @endeAlg t == pure 'True'@ iff @pure j@ is the result
-- of 'decrypt'ing a 'StdMess' @j@ which is encrypted with @t@.
endeAlg :: AlGoreRhythm -> IO Bool;
endeAlg a = (== Right unenc) <$> ende a
  where
  unenc = Jam.stdMess
  ende b = (>>= flip decrypt deets) <$> encrypt b unenc der deets;

-- | @deets@ is the authorisation/keyring crap which is used to encrypt
-- and decrypt messages and whatnot.
deets :: Auth;
deets = Jam.user;

-- | @der@ is the 'Room' which is used within this module.
der :: Room;
der = Jam.room;

-- | @pri@ is the 'PrivateKey' which is used within this module.
pri :: PrivateKey;
pri = "prion";

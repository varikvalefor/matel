-- | Module    : Main
-- Description : Business end of Matel
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the most high-level source code of the Matel
-- TUI client.
--
-- No documentation regarding the user interface is provided; the
-- layout of Matel's user interface is unknown.

module Main where
import TUI;
import Plegg;
import GetAuth;
import Metal.Auth;
import Metal.Base;
import Control.Concurrent;

-- | Tap tap tap
--
-- Keyboard tap tap
--
-- Documentation?
--
-- Write your own crap.
main :: IO ();
main =
  univac >> plegg >>
  getAuthorisationDetails >>= \aufFile ->
  newEmptyMVar >>= \comVar ->
  forkIO (fetchData comVar aufFile) >> summonTUI comVar;

-- | @fetchData k@ collects data from Matrix, parses this data
-- appropriately, and outputs this data to @k@.
--
-- @fetchData@ is currently unimplemented.  @fetchData k@ just sends a
-- placeholder message to @k@.
fetchData :: MVar Winda
          -- ^ This argument is the thing which is used to communicate
          -- with the TUI.
          -> Auth
          -- ^ This argument is the authorisation information which is
          -- used to actually access Matrix.
          -> IO ();
fetchData v a = putMVar v temporaryMessage;

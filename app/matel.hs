-- | This module contains the most high-level source code of the Matel
-- TUI client.
--
-- No documentation regarding the user interface is provided; the
-- layout of the user interface is unknown.

module Main where
import Metal.Auth;
import Metal.Base;
import Control.Concurrent;
import TUI;

-- | Tap tap tap
--
-- Keyboard tap tap
--
-- Documentation?
--
-- Write your own crap.
main :: IO ();
main =
  getAuthorisationDetails >>= \aufFile ->
  newEmptyMVar >>= \comVar ->
  forkIO (fetchData comVar aufFile) >> summonTUI comVar;

-- | For all 'MVar' 'Winda' @k@, @fetchData k@ collects data from
-- Matrix, parses this data appropriately, and outputs this data to @k@.
--
-- @fetchData@ is currently unimplemented.
fetchData :: MVar Winda
          -- ^ The variable which is used to communicate with the TUI
          -> Auth
          -- ^ The authorisation deets
          -> IO ();
fetchData v a = putMVar v temporaryMessage;

import Metal.Base;
import Control.Concurrent;
import GUI;

-- | Tap tap tap
-- Keyboard tap tap
-- Documentation?
-- Write your own crap.
main :: IO ();
main = newEmptyMVar >>= \comVar ->
  forkIO (fetchData comVar) >> summonTUI comVar;

-- | For all MVar Winda k, fetchData k collects data from Matrix, parses
-- this data appropriately, and outputs this data to k.
-- fetchData is currently unimplemented.
fetchData :: MVar Winda -> IO ();
fetchData = flip putMVar temporaryMessage;

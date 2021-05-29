import Metal.Base;
import Control.Concurrent;
import GUI;

-- | Tap tap tap
-- Keyboard tap tap
-- Documentation?
-- Get yer own, twit.
main :: IO ();
main = newEmptyMVar >>= \ comVar ->
  forkIO (fetchData comVar) >> summonTUI comVar;

-- | For all MVar Winda k, fetchData k collects data from Matrix, parses
-- this data appropriately, and outputs this data to k.
fetchData :: MVar Winda -> IO ();
fetchData comVar = putMVar comVar temporaryMessage;

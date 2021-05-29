import Metal.Base;
import Control.Concurrent;
import GUI;

main :: IO ();
main = newEmptyMVar >>= \ comVar ->
  forkIO (fetchData comVar) >> summonTUI comVar;

fetchData :: MVar Winda -> IO ();
fetchData comVar = putMVar comVar temporaryMessage;

{- This module contains functions which directly or nearly directly
 - interface with the Matrix API. -}

module Metal.MatrixAPI where
import Metal.Base;

-- HIGH-LEVEL CRAP -----------------------------------------------------
nRecentMessages :: Integer -> Room -> IO [TextMess];
nRecentMessages n rm = return [];
{- For all Integer n, for all Room rm, nRecentMessages fetches the n
 - most recent messages from rm, outputting the unencrypted messages. -}

-- LOWER-LEVEL CRAP ----------------------------------------------------
nothingHereYetStayTuned :: ();
nothingHereYetStayTuned = ();

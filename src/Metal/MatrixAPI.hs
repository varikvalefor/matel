{- |
 - Module      :  $Header$
 - Description :  $Header$ contains functions for the Matrix API.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains functions which directly or nearly directly
 - interface with the Matrix API via HTTP requests.
 - -}

module Metal.MatrixAPI where
import Metal.Base;

-- HIGH-LEVEL CRAP -----------------------------------------------------
-- | For all Integer n, for all Room rm, nRecentMessages n rm fetches
-- the n most recent messages from rm, outputting the unencrypted
-- messages.
nRecentMessages :: Integer -> Room -> IO [TextMess];
nRecentMessages n rm = return [];

-- LOWER-LEVEL CRAP ----------------------------------------------------
nothingHereYetStayTuned :: ();
nothingHereYetStayTuned = ();

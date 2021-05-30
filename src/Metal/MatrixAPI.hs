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
-- | memberRooms equals the IO-monadic list of all rooms of which
-- Matel's user is a member.
memberRooms :: IO [Room];
memberRooms = return [];

-- LOWER-LEVEL CRAP ----------------------------------------------------
-- | That nothingHereYetStayTuned exists implies that Matel is currently
-- useless as a Matrix client.
-- nothingHereYetStayTuned is removed when proper Matrix API support is
-- added to Matel.
nothingHereYetStayTuned :: ();
nothingHereYetStayTuned = ();

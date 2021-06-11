{- |
 - Module      :  $Header$
 - Description :  $Header$ contains "high-level" functions for Matrix.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains functions which use the Matrix API by chaining
 - together relatively low-level functions for the Matrix API.
 - -}

module Metal.MatrixAPI.HighLevel where
import Metal.Base;
import Metal.Room;
import Data.ByteString (pack);
import Metal.Messages.Standard;
import Metal.MatrixAPI.LowLevel;

-- | For all Integer n, for all Room rm, n `recentMessagesFrom` rm
-- fetches the n most recent text-based messages from rm, outputting the
-- unencrypted/decrypted messages.
recentMessagesFrom :: Integer -> Room -> IO [StdMess];
recentMessagesFrom n rm = return [];

-- | memberRooms equals the IO-monadic list of all rooms of which
-- Matel's user is a member.
-- markRead is currently nonfunctional.
memberRooms :: IO [Room];
memberRooms = return [];

-- | For all (Room k, MessageText g), g `isSentToRoom` k only if a
-- message whose body is g is sent to Matrix room k.
-- markRead is currently nonfunctional.
isSentToRoom :: MessageText -> Room -> IO ErrorCode;
isSentToRoom ms rm = error "isSentToRoom is unimplemented.";

-- | For all messages k, markRead k marks k as having been read.
-- markRead is currently nonfunctional.
markRead :: Mess a => a -> IO ErrorCode;
markRead k = error "markRead is unimplemented.";

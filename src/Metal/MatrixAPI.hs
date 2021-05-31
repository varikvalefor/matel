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
import Metal.Messages.Standard;
import Data.ByteString (pack);

-- HIGH-LEVEL CRAP -----------------------------------------------------
-- | For all Integer n, for all Room rm, n `recentMessagesFrom` rm
-- fetches the n most recent text-based messages from rm, outputting the
-- unencrypted/decrypted messages.
recentMessagesFrom :: Integer -> Room -> IO [StdMess];
recentMessagesFrom n rm = return [];
-- | memberRooms equals the IO-monadic list of all rooms of which
-- Matel's user is a member.
memberRooms :: IO [Room];
memberRooms = return [];
-- | For all (Room k, MessageText g), g `isSentToRoom` k only if a
-- message whose body is g is sent to Matrix room k.
isSentToRoom :: MessageText -> Room -> IO ();
isSentToRoom ms rm = return ();

-- LOWER-LEVEL CRAP ----------------------------------------------------
-- | That stillUnfinishedStayTuned exists implies that Matel is
-- currently useless as a Matrix client.
-- stillUnfinishedStayTuned is removed when proper Matrix API support is
-- added to Matel.
stillUnfinishedStayTuned :: ();
stillUnfinishedStayTuned = ();
-- For all (ByteData z, PublicKey k), encryptWKey z k encrypts z with k,
-- outputting the resulting ciphertext.
-- encryptWKey is currently nonfunctional.
encryptWKey :: ByteData -> PublicKey -> CipherByteData;
encryptWKey text key = pack [];
-- For all (CipherByteData z, PrivateKey k), decryptTextWKey z k
-- decrypts z with k, outputting the resulting ByteData-based data.
-- decryptWKey is currently nonfunctional.
decryptWKey :: CipherByteData -> PrivateKey -> ByteData;
decryptWKey crip key = pack [];

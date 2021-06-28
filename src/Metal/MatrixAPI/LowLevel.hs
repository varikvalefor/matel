{- |
 - Module      :  $Header$
 - Description :  $Header$ contains "low-level" functions for Matrix.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains functions which directly access the Matrix API via
 - HTTP requests, as opposed to being abstracted.
 - -}

module Metal.MatrixAPI.LowLevel where
import Metal.Base;
import Metal.Room;
import Metal.Messages.Standard;
import Data.ByteString (pack);

-- | That @stillUnfinishedStayTuned@ exists implies that Matel is
-- currently useless as a Matrix client.
-- @stillUnfinishedStayTuned@ is removed when proper Matrix API support
-- is added to Matel.
stillUnfinishedStayTuned :: ();
stillUnfinishedStayTuned = ();

-- For all @('ByteData' z, 'PublicKey' k)@, @encryptWKey z k@ encrypts
-- @z@ with @k@, outputting the resulting ciphertext.
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData -> PublicKey -> CipherByteData;
encryptWKey text key = pack [];

-- For all @('CipherByteData' z, 'PrivateKey' k)@, @decryptTextWKey z k@
-- decrypts @z@ with @k@, outputting the resulting 'ByteData'-based
-- data.
-- @decryptWKey@ is currently nonfunctional.
decryptWKey :: CipherByteData -> PrivateKey -> ByteData;
decryptWKey crip key = pack [];

-- | For all valid Matrix usernames @k@, for all valid accompanying
-- passwords @p@, @login k p@ fetches an authorisation token for Matrix
-- account @k@.
login :: Identifier -> Stringth -> IO ErrorCode;
login youshallnot pass = return "login is unimplemented.";

-- | @sendSync@ accesses the Matrix "sync" function.
--
-- @sendSync ""@ fetches the most recent Matrix messages.
-- For all other @k@, @sendSync k@ sends a "sync" query to Matrix such
-- that the "since" parameter of this query equals @k@, fetching all
-- messages which are sent after @k@.
sendSync :: String -- ^ The desired value of the query's "since" field
         -> String -- ^ The domain name of the user's homeserver
         -> IO [StdMess];
sendSync since homsv = return [];

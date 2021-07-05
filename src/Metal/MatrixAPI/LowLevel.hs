{-# LANGUAGE OverloadedStrings #-}

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
import Data.Maybe;
import qualified Data.Aeson as A;
import Network.HTTP.Simple;
import Data.List (elemIndex);
import Metal.Messages.Standard;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.ByteString as BS;

-- | That @stillUnfinishedStayTuned@ exists implies that Matel is
-- currently useless as a Matrix client.
--
-- @stillUnfinishedStayTuned@ is removed when proper Matrix API support
-- is added to Matel.
stillUnfinishedStayTuned :: ();
stillUnfinishedStayTuned = ();

-- For all @('ByteData' z, 'PublicKey' k)@, @encryptWKey z k@ encrypts
-- @z@ with @k@, outputting the resulting ciphertext.
--
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData -> PublicKey -> CipherByteData;
encryptWKey text key = BS.pack [];

-- For all @('CipherByteData' z, 'PrivateKey' k)@, @decryptTextWKey z k@
-- decrypts @z@ with @k@, outputting the resulting 'ByteData'-based
-- data.
--
-- @decryptWKey@ is currently nonfunctional.
decryptWKey :: CipherByteData -> PrivateKey -> ByteData;
decryptWKey crip key = BS.pack [];

-- | For all valid Matrix usernames @k@, for all valid accompanying
-- passwords @p@, @login k p@ fetches an authorisation token for Matrix
-- account @k@.
--
-- The first element of @login@'s 2-tuple equals @""@ iff an
-- authorisation token is successfully fetched and stored in the second
-- element of @login@'s 2-tuple.  This element otherwise equals an
-- explanation of the failure to fetch the authorisation token.
loginPass :: Identifier -> Stringth -> IO (ErrorCode, String);
loginPass user pass =
  generateRequest >>= httpBS >>= BS.putStrLn . getResponseBody >>
  error "loginPass is still not completely implemented."
  where
  generateRequest :: IO Request
  generateRequest = print (A.encode logreq) >>
    parseRequest ("https://" ++ (homeserver ++ "/_matrix/client/r0/login")) >>=
    return . setRequestBodyJSON logreq
  homeserver :: String
  homeserver = drop (fromJust (elemIndex ':' user) + 1) user
  logreq :: LoginRequest
  logreq = LoginRequest {
    lrq_type = "m.login.password",
    lrq_identifier = UserIdentifier {
      usident_type = "m.id.user",
      usident_user = drop 1 $ take (fromJust (elemIndex ':' user)) user
    },
    lrq_password = pass,
    lrq_initdispname = "Matel"
  };

-- | @sendSync@ accesses the Matrix "sync" function.
--
-- @sendSync ""@ fetches the most recent Matrix messages.
-- For all other @k@, @sendSync k@ sends a "sync" query to Matrix such
-- that the "since" parameter of this query equals @k@, fetching all
-- messages which are sent after @k@.
sendSync :: String -- ^ The desired value of the query's "since" field
         -> String -- ^ The domain name of the user's homeserver
         -> IO [StdMess];
sendSync since homsv = error "sendSync is unimplemented.";

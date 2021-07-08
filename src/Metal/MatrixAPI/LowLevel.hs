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
import Metal.User;
import Data.Maybe;
import Data.Text.Encoding;
import qualified Data.Aeson as A;
import Network.HTTP.Simple;
import Data.List (elemIndex);
import Metal.Messages.Standard;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

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

-- | For all 'User' @k@, if @username k@ and @password k@ are
-- set, then @login k@ fetches an authorisation token for Matrix user
-- @k@.
--
-- The 'Right' value of @loginPass k p@ equals the authorisation token
-- which results from signing in to Matrix.  The 'Left' value of
-- @loginPass k p@ exists only if an error is present... and equals a
-- description of such an error.
loginPass :: User -> IO (Either String String);
loginPass user =
  generateRequest >>= httpBS >>= \serverResponse ->
  if getResponseStatusCode serverResponse == 200
    then return $ Right $ toString $ getResponseBody serverResponse
    else return $ Left $ "Thus spake the homeserver: " ++
      (show $ getResponseStatusCode serverResponse) ++ "."
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("POST https://" ++ homeserver user ++ "/_matrix/client/r0/login") >>=
    return . setRequestBodyJSON logreq
  --
  logreq :: LoginRequest
  logreq = LoginRequest {
    lrq_type = "m.login.password",
    lrq_identifier = UserIdentifier {
      usident_type = "m.id.user",
      usident_user = username user
    },
    lrq_password = password user,
    lrq_initdispname = "Matel"
  }
  --
  toString :: BS.ByteString -> String
  toString = map (toEnum . fromEnum) . BS.unpack;

-- | @sendSync@ accesses the Matrix "sync" function.
--
-- @sendSync Nothing g@ runs a parameterless "sync".
-- For all other @k@, @sendSync k g@ sends a "sync" query to Matrix such
-- that the "since" parameter of this query equals @fromJust k@.
--
-- The 'Right' value of @sendSync k g@ equals the authorisation token
-- which results from signing in to Matrix.  The 'Left' value of
-- @loginPass k g@ exists only if an error is present... and equals a
-- description of such an error.
sendSync :: Maybe String -- ^ The desired value of the query's "since" field
         -> User -- ^ The authorisation deets
         -> IO (Either String String);
sendSync since user =
  generateRequest >>= httpBS >>= \serverResponse ->
  if getResponseStatusCode serverResponse == 200
    then return $ Right $ toString $ getResponseBody serverResponse
    else return $ Left $ "Thus spake the homeserver: " ++
      (show $ getResponseStatusCode serverResponse) ++ "."
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("GET https://" ++ homeserver user ++ "/_matrix/client/r0/sync") >>=
    return . addRequestHeader "Authorization" authToken' . setRequestBodyLBS syncreq
  --
  syncreq :: BSL.ByteString
  syncreq
    | isNothing since = ""
    | otherwise = fromString $ "{\"since\": \"" ++ fromJust since ++ "\"}"
  --
  toString :: BS.ByteString -> String
  toString = map (toEnum . fromEnum) . BS.unpack
  --
  authToken' :: BS.ByteString
  authToken' = BSL.toStrict $ fromString $ "Bearer " ++ authToken user
  --
  fromString :: String -> BSL.ByteString
  fromString = BSL.pack . map (toEnum . fromEnum);

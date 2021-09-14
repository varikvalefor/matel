-- | Module    : Metal.MatrixAPI.LowLevel.Crypto
-- Description : Cryptographic crap for Matrix
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.MatrixAPI.LowLevel.Crypto contains cryptographic crap.
module Metal.MatrixAPI.LowLevel.Crypto (
  calcSecret
) where
import Data.Maybe;
import Metal.Base;
import Crypto.Error;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Data.ByteString as BS;
import qualified Crypto.PubKey.Curve25519 as X25519;

-- | @calcSecret a b@ is the shared secret key of @a@ and @b@.
calcSecret :: PublicKey
           -- ^ The public key of the recipient of the data with which
           -- the resulting shared secret is encrypted
           -> PrivateKey
           -- ^ The private key of the sender of the data with which the
           -- resulting shared secret is encrypted
           -> X25519.DhSecret;
calcSecret pu pr = X25519.dh pu' pr'
  where
  toBS :: T.Text -> BS.ByteString
  toBS = fromString . T.unpack
  --
  pu' = yield $ X25519.publicKey $ toBS pu
  --
  pr' = yield $ X25519.secretKey (fromString $ T.unpack pr :: BS.ByteString)
  --
  yield :: CryptoFailable a -> a
  yield = fromJust . maybeCryptoError;

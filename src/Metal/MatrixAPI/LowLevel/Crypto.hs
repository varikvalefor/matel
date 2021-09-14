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
  encryptWKey,
  decryptWKey,
  calcSecret
) where
import Data.Maybe;
import Metal.Base;
import Crypto.Error;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Data.ByteString as BS;
import qualified Crypto.PubKey.Curve25519 as X25519;

-- | @encryptWKey z pu pr@ encrypts @z@ with the shared secret of public
-- key @pu@ and private key @pr@, outputting the resulting ciphertext.
--
-- @encryptWKey@ is IO-monadic because @encryptWKey@ uses functions
-- which return pseudorandom stuff, e.g., the AES-256 keys which encrypt
-- the actual data.
--
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData
            -- ^ The plaintext which should be encrypted
            -> PublicKey
            -- ^ The public key of the recipient of the encrypted thing
            -> PrivateKey
            -- ^ The private key of the sender of the encrypted thing
            -> IO CipherByteData;
encryptWKey text pu pr = error "encryptWKey is unimplemented.";

-- | @decryptWKey z pu pr@ decrypts @z@ with the shared secret of public
-- key @pu@ and private key @pr@, outputting the resulting cleartext.
--
-- @decryptWKey@ is currently nonfunctional.
decryptWKey :: CipherByteData
            -- ^ The ciphertext which should be decrypted
            -> PublicKey
            -- ^ The public key of the sender of the encrypted thing
            -> PrivateKey
            -- ^ The private key of the recipient of the encrypted thing
            -> ByteData;
decryptWKey crip pu pr = error "decryptWKey is unimplemented.";

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

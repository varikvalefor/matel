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
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import qualified Crypto.PubKey.Curve25519 as X25519;
import Crypto.Error;

-- | @encryptWKey z pu pr@ encrypts @z@ with the shared secret of public
-- key @pu@ and private key @pr@, outputting the resulting ciphertext.
--
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData
            -- ^ The plaintext which should be encrypted
            -> PublicKey
            -- ^ The public key of the recipient of the encrypted thing
            -> PrivateKey
            -- ^ The private key of the sender of the encrypted thing
            -> CipherByteData;
encryptWKey text pu pr = T.pack [];

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
decryptWKey crip pu pr = T.pack [];

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

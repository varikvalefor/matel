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
) where
import Metal.Base;
import qualified Data.Text as T;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | @encryptWKey z k@ encrypts @z@ with the public key @k@, outputting
-- the resulting ciphertext.
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

-- | @decryptWKey z k@ decrypts @z@ with @k@, outputting the
-- resulting 'ByteData'-based data.
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

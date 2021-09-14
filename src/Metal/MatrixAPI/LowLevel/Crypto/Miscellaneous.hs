-- | Module    : Metal.MatrixAPI.LowLevel.Crypto.Miscellaneous
-- Description : Miscellaneous cryptographic functions
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- This module contains miscellaneous cryptographic functions.
module Metal.MatrixAPI.LowLevel.Crypto.Miscellaneous where
import Data.Maybe;
import Metal.Base;
import Crypto.Error;
import Crypto.Cipher.AES;
import Crypto.Cipher.Types;
import Crypto.Random.Types;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.ByteString as BS;
import qualified Crypto.PubKey.Curve25519 as X25519;

-- | If @t@ is ciphertext, then @aes256CryptBS t sk iv@ is the result of
-- decrypting @t@ with the secret key @sk@ and the initialisation vector
-- @sk@.
--
-- If @t@ is cleartext, then @aes256CryptBS t sk iv@ is the result of
-- encrypting @t@ with the secret key @sk@ and the initialisation vector
-- @sk@.
-- secret key @b@, returning the resulting ciphertext.
aes256CryptBS :: BS.ByteString
              -- ^ The cleartext
              -> BS.ByteString
              -- ^ The 32-byte secret key
              -> BS.ByteString
              -- ^ The 32-byte initialisation vector
              -> BS.ByteString;
aes256CryptBS t sk iv = ctrCombine cipher (fromJust $ makeIV iv) t
  where
  -- \| A HOPEFULLY INTERESTING NOTE: Unlike many other type
  -- specifications which exist within the "@where@" clauses of Metal,
  -- this type specification is actually necessary.
  --
  -- Go ahead and try to compile this module after removing the type
  -- specification.
  cipher :: AES256
  cipher = fromJust $ maybeCryptoError $ cipherInit sk;

-- | @genIVorKeyBS@ returns a pseudorandom 'BS.ByteString' which is
-- suitable for use as an AES initialisation vector or secret key.
genIVorKeyBS :: IO BS.ByteString;
genIVorKeyBS = getRandomBytes $ blockSize (undefined :: AES256);

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

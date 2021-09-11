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
import Crypto.Error;
import Crypto.Cipher.AES;
import Crypto.Cipher.Types;
import Crypto.Random.Types;
import qualified Data.ByteString as BS;

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
aes256CryptBS pt sk iv = ctrCombine cipher (fromJust $ makeIV iv) pt
  where
  cipher :: AES256
  cipher = fromJust $ maybeCryptoError $ cipherInit sk;

-- | @genIVorKeyBS@ returns a pseudorandom 'BS.ByteString' which is
-- suitable for use as an AES initialisation vector or secret key.
genIVorKeyBS :: IO BS.ByteString;
genIVorKeyBS = getRandomBytes $ blockSize (undefined :: AES256);

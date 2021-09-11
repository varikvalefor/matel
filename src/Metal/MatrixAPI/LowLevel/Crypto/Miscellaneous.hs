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
import Data.ByteArray;
import Crypto.Cipher.AES;
import Crypto.Cipher.Types;
import Crypto.Random.Types;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import qualified Crypto.PubKey.Curve25519 as X25519;

-- | @aes256CryptBS a b@ encrypts the cleartext @a@ with the 32-byte
-- secret key @b@, returning the 2-tuple of the ciphertext and the
-- initialisation vector.
aes256CryptBS :: BS.ByteString
              -- ^ The cleartext
              -> BS.ByteString
              -- ^ The 32-byte secret key
              -> IO (BS.ByteString, BS.ByteString);
aes256CryptBS pt sk = process <$> ivRandomBytes
  where
  ivRandomBytes :: IO BS.ByteString
  ivRandomBytes = getRandomBytes $ blockSize (undefined :: AES256)
  --
  cipher :: AES256
  cipher = fromJust $ maybeCryptoError $ cipherInit sk
  --
  process :: BS.ByteString -> (BS.ByteString, BS.ByteString)
  process iv = (ctrCombine cipher (fromJust $ makeIV iv) pt, iv);

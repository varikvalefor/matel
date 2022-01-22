-- | Module    : Metal.MatrixAPI.LowLevel.Crypto.Miscellaneous
-- Description : Miscellaneous cryptographic functions
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- This module contains miscellaneous cryptographic functions.
module Metal.MatrixAPI.LowLevel.Crypto.Miscellaneous (
  aes256CryptBS,
  calcSecret,
  genIVorKeyBS
) where
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

-- | @aes256CryptBS@ 'Just' encrypts or decrypts a value in accordance
-- with AES-256... unless something goes wrong, in which case 'Nothing'
-- is output.
aes256CryptBS :: BS.ByteString
              -- ^ This value is the thing which should be encrypted
              -- or decrypted.
              --
              -- If this value is encrypted, then the decrypted version
              -- of this value is output.
              --
              -- If this value is unencrypted, then the encrypted
              -- version of this value is output.
              -> BS.ByteString
              -- ^ This thing is the secret key which is used to encrypt
              -- or decrypt the input text.
              -> BS.ByteString
              -- ^ This bit is the initialisation vector.
              -> Maybe BS.ByteString;
aes256CryptBS t sk iv = (\ndl -> ctrCombine cipher ndl t) <$> makeIV iv
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

-- | @calcSecret@ calculates a shared secret key.
calcSecret :: PublicKey
           -- ^ This argument is the public X25519 key of the recipient
           -- of the data which is encrypted using the output shared secret.
           -> PrivateKey
           -- ^ This value is the private X25519 key of the sender of
           -- the data with which is encrypted using the output shared
           -- secret.
           -> X25519.DhSecret;
calcSecret pu pr = X25519.dh pu' pr'
  where
  toBS :: T.Text -> BS.ByteString
  toBS = fromString . T.unpack
  --
  pu' = yield $ X25519.publicKey $ toBS pu
  --
  pr' = yield $ X25519.secretKey (fromString $ T.unpack pr :: BS.ByteString)
  -- \^ This BS.ByteString typecast is necessary; if this typecast is
  -- not present, then GHC complains about the ambiguous typing of the
  -- argument of @X25519.secretKey@ and refuses to compile this module.

  -- \| Using @fromJust@ is the relatively clean approach.  However,
  -- @fromJust@'s error messages are not known for being particularly
  -- conducive to debugging; turning "fromJust is applied to Nothing"
  -- into a useful bug report as a layman is fairly difficult.
  yield :: CryptoFailable a -> a
  yield = fromMaybe nothingMsg . maybeCryptoError
  --
  nothingMsg = error "Something goes horribly, horribly wrong.  \
                     \maybeCryptoError is Nothing.";

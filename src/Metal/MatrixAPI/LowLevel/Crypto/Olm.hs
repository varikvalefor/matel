-- | Module    : Metal.LowLevel.MatrixAPI.Crypto.Olm
-- Description : Implementation of Olm cryptographic protocol
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.LowLevel.MatrixAPI.Crypto.Olm contains some functions which
-- implement the Olm cryptographic protocol such that messages and
-- whatnot can be properly encrypted.
module Metal.LowLevel.MatrixAPI.Crypto.Olm (
  encryptWKey,
  decryptWKey
) where
import Data.Maybe;
import Metal.Base;
import Crypto.Error;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Data.ByteString as BS;
import qualified Crypto.PubKey.Curve25519 as X25519;

-- | @encryptWKey z pu pr@ encrypts @z@ with the shared secret of public
-- key @pu@ and private key @pr@, returning the resulting ciphertext.
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
--
-- = Output
--
-- If the decryption is successful, then the 'ByteData'-based
-- representation of the cleartext is returned.  If something breaks,
-- then a description of this breakage is output as a 'Left'
-- 'ErrorCode'.
decryptWKey :: CipherByteData
            -- ^ The ciphertext which should be decrypted
            -> PublicKey
            -- ^ The public key of the sender of the encrypted thing
            -> PrivateKey
            -- ^ The private key of the recipient of the encrypted thing
            -> Either ErrorCode ByteData;
decryptWKey _ _ _ = Left $ fromString "decryptWKey is unimplemented.";

-- | @hkdf a b c d@ is, according to the Olm specification, "the
-- HMAC-based key derivation function with a salt value of @a@, input
-- key material of @b@, context string @c@, and output keying material
-- length of @d@ bytes".
--
-- The wording of this documentation is a bit weird because the Olm
-- specification's documentation of the function which @hkdf@ implements
-- is a bit weird.
--
-- = Output
--
-- If the key derivation is successful, then a 'ByteData'-based
-- representation of the generated keying material is returned.
-- Otherwise, a 'Left' 'ErrorCode' which describes some sort of
-- error is output.
hkdf :: ByteData
     -- ^ The salt
     -> ByteData
     -- ^ The shared secret
     -> ByteData
     -- ^ The context string
     -> Integer
     -- ^ The byte-based length of the output keying material
     -> Either ErrorCode ByteData;
hkdf _ _ _ _ = Left $ fromString "hkdf is unimplemented.";

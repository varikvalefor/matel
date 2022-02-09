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
            -- ^ This valut is the cleartext which should be encrypted.
            -> PublicKey
            -- ^ This value is the public key of the desired recipient
            -- of the ciphertext.
            -> PrivateKey
            -- ^ This thing is the private key of the user which creates
            -- the ciphertext.
            -> IO CipherByteData;
encryptWKey text pu pr = error "encryptWKey is unimplemented.";

-- | @decryptWKey z pu pr@ decrypts @z@ with the shared secret of public
-- key @pu@ and private key @pr@, outputting the resulting cleartext.
--
-- @decryptWKey@ is currently nonfunctional.
decryptWKey :: CipherByteData
            -- ^ This value is the ciphertext which should be decrypted.
            -> PublicKey
            -- ^ This thing is the public key of the user which creates
            -- the ciphertext which should be decrypted.
            -> PrivateKey
            -- ^ This thing is the private key of the user for which the
            -- data is encrypted.
            -> ByteData;
decryptWKey crip pu pr = error "decryptWKey is unimplemented.";

-- | @hkdf a b c d@ is, according to the Olm specification, "the
-- HMAC-based key derivation function with a salt value of @a@, input
-- key material of @b@, context string @c@, and output keying material
-- length of @d@ bytes".
--
-- The wording of this documentation is a bit weird because the Olm
-- specification's documentation of the function which @hkdf@ implements
-- is a bit weird.
hkdf :: ByteData
     -- ^ This argument is the salt.
     -> ByteData
     -- ^ This argument is the shared secret.
     -> ByteData
     -- ^ This value is the context string.
     -> Integer
     -- ^ This argument is the byte-based length of the output keying
     -- material.  Blame the specification.
     -> ByteData;
hkdf = error "hkdf is unimplemented.";

{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.LowLevel.MatrixAPI.Crypto.Megolm
-- Description : Implementation of Megolm cryptographic protocol
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.MatrixAPI.LowLevel.Crypto.Megolm contains some functions which
-- implement the Megolm cryptographic protocol such that messages and
-- whatnot can be properly encrypted.
module Metal.MatrixAPI.LowLevel.Crypto.Megolm where
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
-- @encryptWKey@ is currently nonfunctional.
--
-- = Output
--
-- If the encryption is successful, then the 'CipherByteData' which
-- represents the encrypted message is returned.  Otherwise, a 'Left'
-- 'ErrorCode' which explains the error which @encryptWKey@ encounters
-- is output.
--
-- @encryptWKey@ is IO-monadic because @encryptWKey@ uses functions
-- which return pseudorandom stuff, e.g., the AES-256 keys which encrypt
-- the actual data.
encryptWKey :: ByteData
            -- ^ This valut is the cleartext which should be encrypted.
            -> PublicKey
            -- ^ This value is the public key of the desired recipient
            -- of the ciphertext.
            -> PrivateKey
            -- ^ This thing is the private key of the user which creates
            -- the ciphertext.
            -> IO (Either ErrorCode CipherByteData);
encryptWKey _ _ _ = pure $ Left "encryptWKey is unimplemented.";

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
            -- ^ This value is the ciphertext which should be decrypted.
            -> PublicKey
            -- ^ This thing is the public key of the user which creates
            -- the ciphertext which should be decrypted.
            -> PrivateKey
            -- ^ This thing is the private key of the user for which the
            -- data is encrypted.
            -> Either ErrorCode ByteData;
decryptWKey _ _ _ = Left "decryptWKey is unimplemented.";

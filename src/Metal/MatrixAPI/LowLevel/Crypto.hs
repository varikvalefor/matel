{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.Crypto
-- Description : Cryptographic crap for Matrix
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.MatrixAPI.LowLevel.Crypto contains high-level cryptographic
-- crap.
module Metal.MatrixAPI.LowLevel.Crypto where
import Metal.Base;
import Metal.Encrypted;
import Metal.Messages.Standard;

-- | For all CryptoThing @a@, for all @a@ @b@, there exists a function
-- @decrypt@ such that @decrypt b@ is a value @a@ which represents the
-- unencrypted @b@.
class CryptoThing a where
  -- | @encrypt a pk sk@ encrypts @a@ using a shared secret which is
  -- calculated using the public key @pk@ and the secret key @sk@.
  encrypt :: a
          -- ^ The thing what should be encrypted
          -> PublicKey
          -- ^ The public key of the receiver
          -> PrivateKey
          -- ^ The privatre key of the sender
          -> AlGoreRhythm
          -- ^ The algorithm which is used to encrypt the thing
          -> IO Encrypted
  -- | @decrypt a pu pr@ decrypts the ciphertext @a@ with the shared
  -- secret which is calculated using the public key @pu@ and the
  -- private key @pr@.
  decrypt :: Encrypted
          -- ^ The thing which should be decrypted
          -> PublicKey
          -- ^ The public key of the sender
          -> PrivateKey
          -- ^ The private key of the receiver
          -> a;

instance CryptoThing StdMess where
  encrypt = error "encrypt is unimplemented."
  decrypt ct pu pr = case algorithm ct of
    "m.olm.v1.curve25519-aes-sha2"
      -> error "StdMess's Olm decryption is unimplemented."
    "m.megolm.v1.aes-sha2"
      -> error "StdMess's Megolm decryption is unimplemented."
    _
      -> error "Some weird, unrecognised algorithm is used.";

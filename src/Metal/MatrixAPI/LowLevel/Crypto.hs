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

-- | For all CryptoThing @a@, @a@ represents a Matix event which can
-- be encrypted.
class CryptoThing a where
  -- | @encrypt@ encrypts Matrix events, returning the resulting
  -- 'Encrypted' event.
  encrypt :: a
          -- ^ This argument is a representation of the Matrix event
          -- which should be encrypted.
          -> PublicKey
          -- ^ This argument is the public key of the user for whom the
          -- event should be encrypted.
          -> PrivateKey
          -- ^ This bit is the private ket of the user which encrypts
          -- the Matrix event.
          -> AlGoreRhythm
          -- ^ This argument represents the ratchet which is used to
          -- encrypt the message.
          --
          -- = Possible Values
          --
          -- If this value is 'Olm', then the Olm ratchet, which is
          -- documented at
          -- <https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/olm.md>,
          -- is used.
          --
          -- If this value is 'Megolm', then the Megolm ratchet, which
          -- is documented at
          -- <https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/megolm.md>,
          -- is used.
          -> IO Encrypted
  -- | @decrypt@ decrypts an 'Encrypted' Matrix event, outputting the
  -- decrypted event.
  decrypt :: Encrypted
          -- ^ This bit represents the event which should be decrypted.
          -> PublicKey
          -- ^ This thing is the public key of the user which encrypts
          -- the event.
          -> PrivateKey
          -- ^ This argument is the private key of the user for whom
          -- the message is encrypted.
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

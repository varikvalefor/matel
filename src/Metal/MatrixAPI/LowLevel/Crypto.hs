{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.Crypto
-- Description : Cryptographic crap for Matrix
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
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
import Metal.OftenUsedFunctions;

-- | For all CryptoThing @a@, @a@ represents a Matix event which can
-- be encrypted.
class CryptoThing a where
  -- | @encrypt@ encrypts a Matrix event, returning the resulting
  -- 'Encrypted' event.
  --
  -- = Output
  --
  -- If the encryption is successful, then the encrypted message is
  -- 'Right'ly output.  If something halts and catches fire, then a
  -- description of this fire is returned as a 'Left' 'ErrorCode'.
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
          -> IO (Either ErrorCode Encrypted)
  -- | @decrypt@ decrypts an 'Encrypted' Matrix event, outputting the
  -- decrypted event.
  --
  -- = Output
  --
  -- If the decryption is successful, then the decrypted thing is
  -- 'Right'ly output.  If the decryption is unsuccessful, then the
  -- reason for the lack of success is output as a 'Left' 'ErrorCode'.
  decrypt :: Encrypted
          -- ^ This bit represents the event which should be decrypted.
          -> PublicKey
          -- ^ This thing is the public key of the user which encrypts
          -- the event.
          -> PrivateKey
          -- ^ This argument is the private key of the user for whom
          -- the message is encrypted.
          -> Either ErrorCode a;

instance CryptoThing StdMess where
  encrypt _ _ _ _ = pure $ bork "encrypt is unimplemented.";
  decrypt ct pu pr = case algorithm ct of
    "m.olm.v1.curve25519-aes-sha2"
      -> bork "StdMess's Olm decryption is unimplemented."
    "m.megolm.v1.aes-sha2"
      -> bork "StdMess's Megolm decryption is unimplemented."
    _
      -> bork "Some weird, unrecognised algorithm is used.";

-- | @bork@ generates converts relatively bare-bones error messages
-- into relatively descriptive error messages.
--
-- @bork@ really just prepends the name of this module to the input
-- error message.
bork :: String
     -- ^ This thing is the error message to which the name of this
     -- module should be prepended.
     -> Either ErrorCode a;
bork = Left . fromString . ("Metal.MatrixAPI.LowLevel.Crypto: " ++);

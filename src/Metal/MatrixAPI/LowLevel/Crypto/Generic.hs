{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.Crypto.Generic
-- Description : Somewhat cryptosystem-agnostic cryptographic crap
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- "Metal.MatrixAPI.LowLevel.Crypto.Generic" contains (a
-- decryption function which abstracts away the whole "Olm vs.
-- Megolm" thing) and (an encryption function which simplifies the whole
-- "Olm vs. Megolm" thing), as well as some supporting functions and
-- datatypes.
module Metal.MatrixAPI.LowLevel.Crypto.Generic (
  encrypt,
  decrypt
) where
import Data.Bool;
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.Messages.Standard;
import Metal.Messages.Encrypted;
import qualified Metal.MatrixAPI.LowLevel.Crypto.Olm as O;
import qualified Metal.MatrixAPI.LowLevel.Crypto.Megolm as M;

-- | At this point, just read the definition.
type Decryptable = Either OlmDecryptable MegolmDecryptable;

-- | Where @j@ is some Megolm-encrypted message, @j@ can be represented
-- as a 'MegolmDecryptable' value @k@ such that...
--
-- * The first element of @k@ is the actual ciphertext of @j@.
--
-- * The second element of @k@ is the public key which is used to
-- create @j@.
--
-- * The third element of @k@ is the private key which can be used to
-- decrypt @j@.
type MegolmDecryptable = (CipherByteData, PublicKey, PrivateKey);

-- | Where @j@ is some Olm-encrypted message, @j@ can be represented as
-- an 'OlmDecryptable' value @k@ such that...
--
-- * The first element of @k@ is the actual ciphertext of @j@.
--
-- * The second element of @k@ is the public key which is used to
-- create @j@.
--
-- * The third element of @k@ is the private key which can be used to
-- decrypt @j@.
type OlmDecryptable = (CipherByteData, PublicKey, PrivateKey);

-- | @decrypt@ decrypts Matrix messages.
--
-- = Output
--
-- If the input 'Encrypted' message is successfully decrypted, then the
-- decrypted 'StdMess' is 'Right'ly output.
--
-- If something fails, then a description of this failure is output as a
-- 'Left' 'ErrorCode'.
--
-- = Meat and Potatoes
--
-- Keep looking.  @decrypt@ just selects and runs an appropriate
-- decryption function; "true" decryption logic is /not/ contained
-- within the definition of @decrypt@.
decrypt :: Encrypted
        -- ^ This record is the message which is to be decrypted.
        -> Auth
        -- ^ This value contains the authorisation information of the
        -- user for whom the input message is encrypted.
        -> Either ErrorCode StdMess;
decrypt msg a = getKeys >>= either dOlm dMegolm >>= toStdMess
  where
  toStdMess _ = Left "toStdMess is unimplemented."
  getKeys = bool (olmKeys msg a) (megolmKeys msg a) =<< usesMegolm
  usesMegolm = Left "usesMegolm is unimplemented."
  dOlm (x,y,z) = O.decryptWKey x y z
  dMegolm (x,y,z) = M.decryptWKey x y z;

-- | If the input 'Encrypted' record is encrypted with 'Olm' and the
-- input 'Auth' record contains the 'PrivateKey' which can be used to
-- decrypt the 'Encrypted' message, then a 'Decryptable' version of this
-- 'Encrypted' message is 'Right'ly output.  Otherwise, a 'Left'
-- 'ErrorCode' is output.
olmKeys :: Encrypted
        -- ^ This bit is the 'Encrypted' message which should be
        -- "described".
        -> Auth
        -- ^ This bit is the authorisation information which
        -- hopefully contains the 'PrivateKey' which can be used to
        -- decrypt the specified 'Encrypted' message.
        -> Either ErrorCode Decryptable;
olmKeys _ _ = Left "olmKeys is unimplemented.";

-- | If the input 'Encrypted' record is encrypted with 'Megolm' and the
-- input 'Auth' record contains the 'PrivateKey' which can be used to
-- decrypt the 'Encrypted' message, then a 'Decryptable' version of this
-- 'Encrypted' message is 'Right'ly output.  Otherwise, a 'Left'
-- 'ErrorCode' is output.
megolmKeys :: Encrypted
           -- ^ This bit is the 'Encrypted' message which should be
           -- "described".
           -> Auth
           -- ^ This bit is the authorisation information which
           -- hopefully contains the 'PrivateKey' which can be used to
           -- decrypt the specified 'Encrypted' message.
           -> Either ErrorCode Decryptable;
megolmKeys _ _ = Left "megolmKeys is unimplemented.";

-- | Again, just read the danged definition.
type Encryptable = Either OlmEncryptable MegolmEncryptable;

-- | Where @j@ is some Megolm-encrypted message, there can exist a
-- 'MegolmEncrypted' @k@ such that...
--
-- * The first element of @k@ becomes the actual ciphertext of @j@.
--
-- * The second element of @k@ is the public key which is used to
-- create @j@.
--
-- * The third element of @k@ is the private key which can be used to
-- decrypt @j@.
type MegolmEncryptable = (ByteData, PublicKey, PrivateKey);

-- | Where @j@ is some Olm-encrypted message, there can exist a
-- 'OlmEncrypted' @k@ such that...
--
-- * The first element of @k@ becomes the actual ciphertext of @j@.
--
-- * The second element of @k@ is the public key which is used to
-- create @j@.
--
-- * The third element of @k@ is the private key which can be used to
-- decrypt @j@.
type OlmEncryptable = (ByteData, PublicKey, PrivateKey);

-- | @encrypt@ encrypts the specified message or just 'splodes.
--
-- = Output
--
-- If the encryption is successful, then the 'Right'ly-'Encrypted' message
-- is returned.
--
-- If the encryption fails, then a 'Left' 'ErrorCode'-based description of
-- the failure is returned.
encrypt :: AlGoreRhythm
        -- ^ This value is the name of the cryptosystem which is used to
        -- encrypt the input message.
        -> StdMess
        -- ^ This value is the message which should be encrypted.
        -> Room
        -- ^ This value specifies the Matrix room for which the message is
        -- encrypted.
        -> Auth
        -- ^ This bit is the authorisation information which contains the
        -- keys which are used to actually encrypt the message.
        -> IO (Either ErrorCode Encrypted);
encrypt c s r a = (>>= toEncrypted) <$> eitherCrypt getCryptoCrap'
  where
  eitherCrypt = either (pure . Left) (either oCrypt mCrypt)
  toEncrypted _ = Left "toEncrypted is unimplemented."
  getCryptoCrap' = getCryptoCrap c s r a
  oCrypt (x,y,z) = O.encryptWKey x y z
  mCrypt (x,y,z) = M.encryptWKey x y z;

-- | @getCryptoCrap@ 'Right'ly outputs a value which can be used to
-- conveniently encrypt the specified 'StdMess' or outputs a 'Left'
-- 'ErrorCode' which describes the error which is encountered.
getCryptoCrap :: AlGoreRhythm
              -- ^ This bit is the name of the cryptographic algorithm
              -- which should be used to encrypt the message.
              -> StdMess
              -- ^ This argument is the message which should be
              -- encrypted.
              -> Room
              -- ^ This argument specifies the Matrix room for which the
              -- message should be encrypted.
              -> Auth
              -- ^ This record contains information which is used to
              -- actually encrypt the message, e.g., the 'PrivateKey' of
              -- the encrypting party.
              -> Either ErrorCode Encryptable;
getCryptoCrap _ _ _ _ = Left "getCryptoCrap is unimplemented.";

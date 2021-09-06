-- | Module    : Metal.Messages.EncryptedFile
-- Description : Encrypted file attachment record type
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This file contains 'EncryptedFile' and company.
module Metal.Messages.EncryptedFile where
-- | For all 'EncryptedFile' @k@, @k@ describes an encrypted file.
data EncryptedFile = EncryptedFile {
  -- | @url k@ is the URL of the encrypted file.
  url :: String,
  -- | @key k@ is the key which is used to encrypt the file.
  key :: JWK,
  -- | @iv k@ is the AES-CTR counter block which is used when @k@ is
  -- encrypted.
  iv :: String,
  -- | For all @j@ in @hashes k@, @fst j@ is the name of a hashing
  -- function, and @snd j@ is the result of applying the hashing
  -- function which @fst j@ describes to the ciphertext.
  hashes :: [(String, String)],
  -- | @v k@ is the version number of the encrypted attachments protocol
  -- which is used to encrypt and decrypt @k@.  According to the API
  -- specification, @v k@ must equal @"v2"@.
  v :: String
} deriving (Eq, Read, Show);

-- | For all 'JWK' @a@, @a@ describes a JSON Web key.
data JWK = JWK {
  -- | @kty k@ is the key type of @k@.  According to the API
  -- specification, this thing must equal @"oct"@.
  kty :: String,
  -- | @key_ops k@ is a list of the operations which @k@ supports.
  key_ops :: [String],
  -- | @alg k@ is the algorithm for which @k@ is suitable.  According to
  -- the API specification, this thing must equal @"A256CTR"@.
  alg :: String,
  -- | @k j@ is the actual key portion of @j@.  This bit is encoded as
  -- URL-safe unpadded base64.
  k :: String,
  -- | @ext k@ iff the file is extractable.  Again, according to the
  -- Matrix specification, this bit must equal 'True'.
  ext :: Bool
} deriving (Eq, Read, Show);

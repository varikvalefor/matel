{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.Messages.EncryptedFile
-- Description : Encrypted file attachment record type
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This file contains 'EncryptedFile' and company.
module Metal.Messages.EncryptedFile where
import Data.Aeson;
import Data.Aeson.TH;
import qualified Data.Text as T;
import Data.HashMap.Strict (toList);

-- | For all 'JWK' @a@, @a@ represents a JSON Web key.
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

$(deriveJSON defaultOptions ''JWK);

-- | For all 'EncryptedFile' @k@, @k@ represents an encrypted file.
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

-- | The manual derivation of this 'FromJSON' instance is necessary
-- because 'hashes'\'s type ruins 'deriveJSON'\'s interpretation of
-- 'hashes'.
instance FromJSON EncryptedFile where
  parseJSON = withObject "EncryptedFile" parse
    where
    parse f = do {
      urli <- f .: "url";
      flda <- f .: "key";
      vein <- f .: "iv";
      vktr <- f .: "v";
      hssh <- f .: "hashes";
      return EncryptedFile {
        url    = urli,
        key    = flda,
        iv     = vein,
        hashes = toList hssh,
        v      = vktr
      };
    };

-- | The manual derivation of this 'ToJSON' instance is necessary
-- because 'hashes'\'s type ruins 'deriveJSON'\'s interpretation of
-- 'hashes'.
instance ToJSON EncryptedFile where
  toJSON t = object
    [
      "url"    .= url t,
      "key"    .= key t,
      "v"      .= v t,
      "iv"     .= iv t,
      "hashes" .= object (map fromHash $ hashes t)
    ]
    where
    fromHash (a,b) = T.pack a .= b;

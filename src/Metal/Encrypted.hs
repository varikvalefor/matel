-- | Metal.Messages.Encrypted contains the source code of the
-- 'CryptoMess' record type.
module Metal.Encrypted where
import Metal.Base;
import Metal.EventCommonFields;

-- | For all 'CryptoMess' @k@, @k@ is an encrypted Matrix message.
data Encrypted = Encrypted {
  -- | @ciphertext k@ is the actual encrypted bit of @k@.
  ciphertext :: Stringth,
  -- | @algorithm k@ is the algorithm which is used to encrypt @k@.
  algorithm :: Stringth,
  -- | @device_id k@ equals the content of the "device_id" field of
  -- @k@'s source.
  device_id :: Maybe Stringth,
  -- | @sender_key k@ is the public key of the sender of @k@.
  sender_key :: Stringth,
  -- | @session_id k@ is the ID of the Matrix session which sends @k@.
  session_id :: Maybe Stringth,
  -- | @boilerplate k@ contains the fields which all event types
  -- contain.
  boilerplate :: EventCommonFields
} deriving (Eq, Read, Show);

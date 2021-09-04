-- | Metal.Messages.Encrypted contains the source code of the
-- 'CryptoMess' record type.
module Metal.Encrypted where
import Metal.Base;
import Metal.EventCommonFields;

-- | For all 'CryptoMess' @k@, @k@ is an encrypted Matrix message.
data Encrypted = Encrypted {
  -- | @ciphertext k@ equals the content of the "ciphertext" field of
  -- @k@'s source.
  ciphertext :: Stringth,
  -- | @algorithm k@ equals the content of the "algorithm" field of
  -- @k@'s source.
  algorithm :: Stringth,
  -- | @device_id k@ equals the content of the "device_id" field of
  -- @k@'s source.
  device_id :: Maybe Stringth,
  -- | @sender_key k@ is the public key of the sender of this encrypted
  -- event.
  sender_key :: Stringth,
  -- | @session_id k@ equals the content of the "session_id" field of
  -- @k@'s source, i.e., the ID of the session which sends the event.
  session_id :: Maybe Stringth,
  -- | @boilerplate k@ contains the fields which all event types
  -- contain.
  boilerplate :: EventCommonFields
} deriving (Eq, Read, Show);

{-# LANGUAGE DeriveAnyClass #-}

-- | Metal.Messages.Encrypted contains the source code of the
-- 'CryptoMess' record type.
module Metal.Messages.Encrypted where
import Metal.Base;

-- | For all 'CryptoMess' @k@, @k@ is an encrypted Matrix message.
data CryptoMess = CryptoMess {
  -- | @ciphertext k@ equals the content of the "ciphertext" field of
  -- @k@'s source.
  --
  -- [SNAKE ROAD SIGN] Remember that the decrypted ciphertext may
  -- actually contain an attachment, as opposed to a text message.
  ciphertext :: Stringth,
  -- | @algorithm k@ equals the content of the "algorithm" field of
  -- @k@'s source.
  algorithm :: Stringth,
  -- | @device_id k@ equals the content of the "device_id" field of
  -- @k@'s source.
  device_id :: Stringth,
  -- | @relates_to k@ equals the content of the "event_id" field of the
  -- "m.relates_to" dingus of @k@'s source.
  relates_to :: Stringth,
  -- | @sender_key k@ equals the content of the "sender_key" field of
  -- @k@'s source, i.e., the public key of the sender of the message.
  sender_key :: Stringth,
  -- | @session_id k@ equals the content of the "session_id" field of
  -- @k@'s source, i.e., the ID of the session which sends the message.
  session_id :: Stringth,
  -- | @sender k@ equals the content of the
  -- "sender" field of @k@'s source.
  sender :: Stringth
} deriving (Eq, Read, Show);

{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.Messages.Encrypted
-- Description : Metal's datatype what represents encrypted events
--               of the Matrix instant messaging service
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.Messages.Encrypted contains the source code of the
-- 'Encrypted' record type.
module Metal.Messages.Encrypted where
import Data.Aeson;
import Data.Maybe;
import Metal.Base;
import Metal.EventCommonFields;

-- | For all 'Encrypted' @k@, @k@ represents an encrypted Matrix
-- message.
data Encrypted = Encrypted {
  -- | @ciphertext k@ is the actual encrypted bit of @k@.
  ciphertext :: Stringth,
  -- | @algorithm k@ is the algorithm which is used to encrypt @k@.
  algorithm :: Stringth,
  -- | @device_id k@ is the ID of the device which sends @k@.
  device_id :: Maybe Stringth,
  -- | @sender_key k@ is the public key of the sender of @k@.
  sender_key :: Stringth,
  -- | @session_id k@ is the ID of the Matrix session which sends @k@.
  session_id :: Maybe Stringth,
  -- | @boilerplate k@ contains the fields which all event types
  -- contain.
  boilerplate :: EventCommonFields
} deriving (Eq, Read, Show);

-- This 'ToJSON' instance is placed into this file because GHC complains
-- about "orphan types" if this instance is placed into
-- "Metal.MatrixAPI.LowLevel.Types".
instance ToJSON Encrypted where
  toJSON enk = object
    [
      "algorithm" .= algorithm enk,
      "ciphertext" .= ciphertext enk,
      "sender_key" .= sender_key enk,
      "device_id" .= fromMaybe "" (device_id enk),
      "session_id" .= fromMaybe "" (session_id enk)
    ];

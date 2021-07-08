{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Metal.MatrixAPI.LowLevel.Types where
import Data.Text;
import Data.Aeson;
import Metal.Base;
import GHC.Generics;
import Data.ByteString;
import Data.Text.Encoding;

-- | 'LoginRequest' is used within 'LowLevel' to hold login requests
-- which are to be converted to JSON.
data LoginRequest = LoginRequest {
  lrq_type :: Stringth,
  lrq_identifier :: UserIdentifier,
  lrq_password :: Stringth,
  lrq_initdispname :: Stringth
} deriving (Eq, Generic, Read, Show);

data UserIdentifier = UserIdentifier {
  usident_type :: Stringth,
  usident_user :: String
} deriving (Eq, Generic, Read, Show);

instance ToJSON LoginRequest where
  toJSON (LoginRequest lrq_type lrq_identifier lrq_password lrq_initdispname) =
    object [
      "initial_device_display_name" .= (decodeUtf8 $ lrq_initdispname),
      "password" .= (decodeUtf8 $ lrq_password),
      "identifier" .= object [
        "user" .= usident_user lrq_identifier,
        "type" .= (decodeUtf8 $ usident_type lrq_identifier)
      ],
      "type" .= (decodeUtf8 $ lrq_type)];
instance ToJSON UserIdentifier;

instance ToJSON ByteString where
  toJSON = String . decodeUtf8;

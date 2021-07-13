-- Metal.MatrixAPI.LowLevel.Types contains the datatypes which are of
-- use only within Metal.MatrixAPI.LowLevel.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Metal.MatrixAPI.LowLevel.Types where
import Metal.Base;
import Data.Aeson;
import Metal.Base;
import Data.Aeson.TH;
import Data.Text.Encoding;
import qualified Data.Text as DT;
import qualified Data.ByteString as BS;

-- | 'LoginRequest' is used within 'LowLevel' to hold login requests
-- which are to be converted to JSON.
data LoginRequest = LoginRequest {
  lrq_type :: Stringth,
  lrq_identifier :: UserIdentifier,
  lrq_password :: Stringth,
  lrq_initial_device_display_name :: Stringth
} deriving (Eq, Read, Show);

data UserIdentifier = UserIdentifier {
  usident_type :: Stringth,
  usident_user :: String
} deriving (Eq, Read, Show);

data StringListRoomIdentifier = StringListRoomIdentifier {
  joined_room :: [String]
} deriving (Eq, Read, Show);

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''LoginRequest;

deriveJSON defaultOptions {fieldLabelModifier = drop 8} ''UserIdentifier;

deriveJSON defaultOptions ''StringListRoomIdentifier;

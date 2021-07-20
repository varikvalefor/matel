{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Metal.MatrixAPI.LowLevel.Types contains the datatypes which are of
-- use only within Metal.MatrixAPI.LowLevel.
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

-- | 'UserIdentifier' holds the "user identifier" record which is used
-- when a login request is sent.
data UserIdentifier = UserIdentifier {
  usident_type :: Stringth,
  usident_user :: String
} deriving (Eq, Read, Show);

-- | 'StringListRoomIdentifier' is used to read the output of the
-- "joined rooms" Matrix API query.
data StringListRoomIdentifier = StringListRoomIdentifier {
  joined_room :: [String]
} deriving (Eq, Read, Show);

data DisplayNameResponse = DisplayNameResponse {
  dnr_displayname :: Stringth
} deriving (Eq, Read, Show);

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''LoginRequest;

deriveJSON defaultOptions {fieldLabelModifier = drop 8} ''UserIdentifier;

deriveJSON defaultOptions ''StringListRoomIdentifier;

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''DisplayNameResponse;

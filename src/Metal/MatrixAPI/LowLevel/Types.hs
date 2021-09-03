{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Metal.MatrixAPI.LowLevel.Types contains the datatypes which are of
-- use only within Metal.MatrixAPI.LowLevel.
module Metal.MatrixAPI.LowLevel.Types where
import Data.Aeson;
import Data.Maybe;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Control.Lens hiding ((.=));
import Data.Aeson (FromJSON, fromJSON);
import Data.Aeson.Types (Parser);
import Data.Aeson.TH;
import Data.Aeson.Lens;
import Metal.Encrypted;
import Metal.EventCommonFields;
import Metal.Messages.FileInfo;
import Metal.Messages.Standard;
import qualified Data.Text as T;
import qualified Metal.Default as Def;

-- | For all 'LoginRequest' @k@, @k@ is a login request which is to be
-- converted to JSON.
data LoginRequest = LoginRequest {
  -- | @lrq_type k@ becomes the value of the "type" field of the
  -- generated JSON.
  lrq_type :: Stringth,
  -- | @lrq_identifier k@ becomes the value of the "identifier" field of
  -- the generated JSON.
  lrq_identifier :: UserIdentifier,
  -- | @lrq_password k@ becomes the value of the "password" field of the
  -- generated JSON.
  lrq_password :: Stringth,
  -- | @lrq_initial_device_display_name k@ becomes the value of the
  -- "initial_device_display_name" field of the generated JSON.
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

-- | 'DisplayNameResponse' is used to read the response of the
-- "displayname" Matrix API request.
data DisplayNameResponse = DisplayNameResponse {
  dnr_displayname :: Stringth
} deriving (Eq, Read, Show);

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''LoginRequest;

deriveJSON defaultOptions {fieldLabelModifier = drop 8} ''UserIdentifier;

deriveJSON defaultOptions ''StringListRoomIdentifier;

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''DisplayNameResponse;

instance ToJSON StdMess where
  toJSON k
    | msgType k `elem` [TextInnit, Notice] = object
      [
        "body" .= body k,
        "msgtype" .= show (msgType k)
      ]
    | msgType k == Location = object
      [
        "body" .= body k,
        "geo_uri" .= fromMaybe (errorNoField "geo_uri") (geo_uri k)
      ]
    | msgType k == Attach = object [
        "body" .= body k,
        "filename" .= filename k,
        "info" .= object [
          "mimetype" .= fromMaybe (errorNoField "mimetype") (mimetype <$> fileInfo k),
          "size" .= fromMaybe (errorNoField "size") (size <$> fileInfo k)
        ],
        "msgtype" .= show (msgType k),
        "url" .= url k
      ]
    | otherwise = error $ "A proper error!  ToJSON does not account \
      \for StdMess values of @msgType@ " ++ show (msgType k) ++ "."
    where
    errorNoField :: String -> a
    errorNoField j = error $ "This " ++ show (msgType k) ++ " lacks a " ++ show j ++ "field!";

instance ToJSON Encrypted where
  toJSON k = object
    [
      "algorithm" .= algorithm k,
      "ciphertext" .= ciphertext k,
      "sender_key" .= sender_key k,
      "device_id" .= fromMaybe "" (device_id k),
      "session_id" .= fromMaybe "" (session_id k)
    ];

{-# LANGUAGE OverloadedStrings #-}

-- | Metal.MatrixAPI.LowLevel.ResponseToWhatever contains functions
-- which process the 'Response's of @'httpBS'@, outputting relevant
-- stuff as data of certain types.
--
-- This module is used only within Metal.MatrixAPI.LowLevel and the
-- submodules of Metal.MatrixAPI.LowLevel.
module Metal.MatrixAPI.LowLevel.ResponseToWhatever where
import Metal.Base;
import Data.Text.Encoding;
import Network.HTTP.Simple;
import qualified Data.Text as T;
import qualified Data.ByteString as BS;

-- | @responseToStringth k@ equals a 'Stringth' which describes the
-- status code of @k@.
responseToStringth :: Response a -> Stringth;
responseToStringth r = T.pack $ "Thus spake the homeserver: " ++
  show (getResponseStatusCode r) ++ ".";

-- | If the status code of @k@ equals @200@, then @responseToMaybe k@
-- equals 'Nothing'.  @responseToMaybe k@ otherwise equals the 'String'
-- equivalent of @'responseToStringth' k@.
--
-- This function is added to decrease the amount of boilerplate stuff
-- within this module.
responseToMaybe :: Response BS.ByteString -> Maybe String;
responseToMaybe theResponse
  | getResponseStatusCode theResponse == 200 = Nothing
  | otherwise = Just $ T.unpack $ responseToStringth theResponse;

-- | If the response code of @k@ equals @200@, then
-- @responseToLeftRight k@ equals the response body of @k@.
-- @responseToLeftRight k@ otherwise equals a 'Stringth' which contains
-- the status code of @k@.
responseToLeftRight :: Response BS.ByteString
                    -- ^ The 'Response' whose response code should be
                    -- reported
                    -> Either Stringth Stringth;
responseToLeftRight k
  | getResponseStatusCode k == 200 =
    Right $ decodeUtf8 $ getResponseBody k
  | otherwise = Left $ responseToStringth k;

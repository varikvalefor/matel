{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.ResponseToWhatever
-- Description : HTTP response conversion gubbins
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.MatrixAPI.LowLevel.ResponseToWhatever contains functions
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

-- | @responseToString k@ equals a 'String' which describes the status
-- code of @k@.
responseToString :: Show a
                 => Response a
                 -> String;
responseToString = T.unpack. responseToStringth;

-- | @responseToStringth k@ equals a 'Stringth' which describes the
-- status code of @k@.
responseToStringth :: Show a
                   => Response a
                   -> Stringth;
responseToStringth r = T.pack $ "Thus spake the homeserver: " ++
  show (getResponseStatusCode r) ++ "; " ++ show (getResponseBody r);

-- | If the status code of @k@ equals @200@, then @responseToMaybe k@
-- equals 'Nothing'.  @responseToMaybe k@ otherwise equals the 'String'
-- equivalent of @'responseToStringth' k@.
responseToMaybe :: Response BS.ByteString
                -> Maybe String;
responseToMaybe theResponse = case getResponseStatusCode theResponse of
  200 -> Nothing
  _   -> Just $ T.unpack $ responseToStringth theResponse;

-- | If the response code of @k@ equals @200@, then
-- @responseToLeftRight k@ equals the response body of @k@.
-- @responseToLeftRight k@ otherwise equals a 'Stringth' which contains
-- the status code of @k@.
responseToLeftRight :: Response BS.ByteString
                    -- ^ The 'Response' whose response code should be
                    -- reported
                    -> Either Stringth Stringth;
responseToLeftRight k = case getResponseStatusCode k of
  200 -> Right $ decodeUtf8 $ getResponseBody k
  _   -> Left $ responseToStringth k;

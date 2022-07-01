{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.ResponseToWhatever
-- Description : HTTP response conversion gubbins
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
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

-- | @responseToString k@ is a 'String' which describes the status
-- code of @k@.
--
-- Adherence to the "Show" class is demanded to ensure that the bodies
-- of input responses can be output.
responseToString :: Show a
                 => Response a
                 -> String;
responseToString = T.unpack. responseToStringth;
-- \^ @responseToString@ is a wrapper for @responseToStringth@, as
-- opposed to having @responseToStringth@ be a wrapper for
-- @responseToString@, because as of 20211226, 'String' is mostly
-- deprecated.

-- | @responseToStringth k@ is a 'Stringth' which describes the
-- status code of @k@.
--
-- Adherence to the "Show" class is demanded to ensure that the bodies
-- of input responses can be output.
responseToStringth :: Show a
                   => Response a
                   -> Stringth;
responseToStringth r = T.pack $ "Thus spake the homeserver: " ++
  show (getResponseStatusCode r) ++ "; " ++ show (getResponseBody r);

-- | If the status code of @k@ is @200@, then @responseToMaybe k@
-- is 'Nothing'.  @responseToMaybe k@ otherwise is
-- @'responseToStringth' k@.
responseToMaybe :: Show a
                => Response a
                -> Maybe Stringth;
responseToMaybe theResponse = case getResponseStatusCode theResponse of
  200 -> Nothing
  _   -> Just $ responseToStringth theResponse;

-- @responseToMaybe'@ is a derivative of 'responseToMaybe'
-- whose input is 'Either'-monadic.
--
-- The documentation of 'responseToMaybe' should enlighten.
responseToMaybe' :: Either ErrorCode (Response BS.ByteString)
                 -> Maybe ErrorCode;
responseToMaybe' = either pure responseToMaybe;

-- | If the response code of @k@ is @200@, then
-- @responseToLeftRight k@ is the response body of @k@.
-- @responseToLeftRight k@ otherwise is a 'Stringth' which contains
-- the status code of @k@.
responseToLeftRight :: Response BS.ByteString
                    -- ^ This value is the 'Response' whose diagnostic
                    -- crap should be reported.
                    -> Either Stringth Stringth;
responseToLeftRight k = case getResponseStatusCode k of
  200 -> Right $ decodeUtf8 $ getResponseBody k
  _   -> Left $ responseToStringth k;

-- @responseToLeftRight'@ is a derivative of 'responseToLeftRight'
-- whose input is 'Either'-monadic.
--
-- The documentation of 'responseToLeftRight' should enlighten.
responseToLeftRight' :: Either ErrorCode (Response BS.ByteString)
                     -> Either ErrorCode Stringth;
responseToLeftRight' = either pure responseToLeftRight;

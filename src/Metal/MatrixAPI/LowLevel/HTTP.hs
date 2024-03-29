{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.HTTP
-- Description : Boilerplate HTTP stuff
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains @'req'@ and some stuff which supports @'req'@.
module Metal.MatrixAPI.LowLevel.HTTP where
import Metal.Auth;
import Metal.OftenUsedFunctions;
import Metal.Base;
import Metal.User;
import Control.Monad;
import Network.HTTP.Simple;
import Network.HTTP.Types.Header;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | For all 'ReqType' @k@, @k@ represents the type of a HTTP request.
--
-- These values are described in section 9 of RFC 2616.
data ReqType = GET | POST | PUT deriving Show;

-- | @req@ sends standardised HTTP requests or 'splodes... without
-- /necessarily/ 'sploding the program which uses @req@.
--
-- If the input of @req@ can be used to send a standardised HTTP
-- request, then @req@ uses this information to send such a standardised
-- HTTP request and 'Right'y returns the response for this HTTP request.
-- If such information is /incomplete/, then returned is a 'Left' value
-- which explains such incompleteness.
req :: ReqType
    -- ^ This bit is a representation of the type of the HTTP request
    -- which is sent.
    -> [(HeaderName, BS.ByteString)]
    -- ^ This argument contains any additional headers which are added
    -- to the HTTP request.
    --
    -- For all elements of this list @t@, @t@ represents a HTTP header
    -- whose name is @fst t@ and whose value is @snd t@.
    -> String
    -- ^ This argument is the concatenation of the path of the HTTP
    -- request which should be sent, a question mark, and the query
    -- string of the request which should be sent.
    -> BSL.ByteString
    -- ^ This argument is the body of the HTTP request which should
    -- be sent.
    -> Auth
    -- ^ This argument contains authorisation information.
    --
    -- If a request which demands an authorisation token is to be sent,
    -- then the @authToken@ field must be defined.
    --
    -- If some other request is sent, then the @username@ and @password@
    -- values should /probably/ be defined.
    --
    -- In both cases, the @protocol@ and @homeserver@ values must be
    -- defined and valid.
    -> IO (Either ErrorCode (Response BS.ByteString));
req type_ h q b a = maybe noProt useRequest $ protocol a
  where
  noProt :: IO (Either ErrorCode (Response BS.ByteString))
  noProt = pure $ Left "req: The \"protocol\" content is Nothing."
  --
  useRequest :: Protocol
             -> IO (Either ErrorCode (Response BS.ByteString))
  useRequest = genRequest >=> either (pure . Left) (Right <.> httpBS)
  --
  genRequest :: Protocol -> IO (Either ErrorCode Request)
  genRequest p = addHeaders . addBody <$> parseRequest (prefix p ++ q)
  --
  addBody :: Request -> Request
  addBody = setRequestBodyLBS b
  --
  addHeaders :: Request -> Either ErrorCode Request
  addHeaders j = maybe (Left noAuthToken) usingAT $ authToken' a
    where
    noAuthToken = "req: The \"authToken\" field is Nothing."
    usingAT = Right . foldr (uncurry addRequestHeader) j . headersToAdd
    headersToAdd t = ("Authorization", t):h
  --
  prefix :: Protocol -> String
  prefix p = type_' ++ show p ++ "://" ++ homeserver a ++ "/"
    where type_' = show type_ ++ " ";

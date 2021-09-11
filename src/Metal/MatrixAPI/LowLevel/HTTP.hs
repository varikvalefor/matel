{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.HTTP
-- Description : Boilerplate HTTP stuff
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains @'req'@ and some stuff which supports @'req'@.
module Metal.MatrixAPI.LowLevel.HTTP where
import Metal.Auth;
import Metal.User;
import Network.HTTP.Simple;
import Network.HTTP.Types.Header;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | For all 'ReqType' @k@, @k@ represents the type of a HTTP request.
data ReqType = GET | POST | PUT;

-- | @req type_ vx query body auth@ sends a HTTP request of type @type_@
-- to FQDN @homeserver auth@ such that @auth@ is used as the content of
-- the "Authorization" header of the request and the path and query
-- string of this request are @query@.  Additionally, the headers which
-- are specified in @vx@ are added to the request.
req :: ReqType
    -- ^ The type of request which should be sent
    -> [(HeaderName, BS.ByteString)]
    -- ^ The additional header name/header value pairs which the request
    -- should bear
    -> String
    -- ^ The path and query string of the request which should be sent
    -> BSL.ByteString
    -- ^ The body of the "GET" request
    -> Auth
    -- ^ The authorisation information of Matel's user
    -> IO (Response BS.ByteString);
req type_ headers query body auth = genRequest >>= httpBS
  where
  genRequest :: IO Request
  genRequest = addHeaders . addBody <$> parseRequest (prefix ++ query)
  --
  addBody :: Request -> Request
  addBody = setRequestBodyLBS body
  --
  addHeaders :: Request -> Request
  addHeaders j = foldr (uncurry addRequestHeader) j h
    where h = ("Authorization", authToken' auth):headers
  --
  prefix :: String
  prefix = show type_ ++ " https://" ++ homeserver auth ++ "/";

instance Show ReqType where
  show k = case k of
    GET  -> "GET"
    PUT  -> "PUT"
    POST -> "POST"
    _    -> error "show receives an unknown ReqType.  As a result of \
            \not understanding this ReqType, show halts and catches \
            \fire.  Although the fire is quickly extinguished, the \
            \fire is extinguished with saltwater, and electronic \
            \stuff does not particularly care for saltwater.  As \
            \such, show is now broken.";

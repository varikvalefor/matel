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
import Data.Maybe;
import Metal.Auth;
import Metal.User;
import Network.HTTP.Simple;
import Network.HTTP.Types.Header;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | For all 'ReqType' @k@, @k@ represents the type of a HTTP request.
data ReqType = GET | POST | PUT;

-- | @req@ sends a standardised HTTP request, returning the resulting
-- response.
req :: ReqType
    -- ^ This bit specifies the type of request which is sent.
    -> [(HeaderName, BS.ByteString)]
    -- ^ This bit is a list of headers which are added to the HTTP
    -- request.
    --
    -- For any element of this list @t@, @fst t@ is the name of some
    -- header, and @snd t@ is the content of this same header.
    -> String
    -- ^ This bit is the concatenation of the path, a question mark, and
    -- the query string of the HTTP request.
    -> BSL.ByteString
    -- ^ This bit is the body of the request.
    -> Auth
    -- ^ This argument is the authorisation information of Matel's user.
    -- This authorisation information is used to generate an
    -- "Authorization" header.
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
  addHeaders j = foldr (uncurry addRequestHeader) j headersToAdd
    where headersToAdd = ("Authorization", authToken' auth):headers
  --
  prefix :: String
  prefix = show type_ ++ " " ++ fromJust (protocol auth) ++ "://" ++
           homeserver auth ++ "/";

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
            -- \^ No, GHC, this case is not redundant.  This case exists
            -- to ensure that if some weird new ReqType is added without
            -- receiving a 'Show' instance, then a descriptive error may
            -- be thrown.
            --
            -- Additionally, keeping this thing implies being able to
            -- keep a dumb joke.

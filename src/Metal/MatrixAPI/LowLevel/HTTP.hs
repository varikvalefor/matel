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
import Data.Maybe;
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
data ReqType = GET | POST | PUT;

-- | @req@ sends a standardised HTTP request, returning the response to
-- this HTTP request.
req :: ReqType
    -- ^ This bit is a representation of the type of HTTP request which
    -- is sent.
    -> [(HeaderName, BS.ByteString)]
    -- ^ This argument contains any additional values which are added to
    -- the HTTP request.
    --
    -- For all elements of this list @t@, @t@ represents a HTTP header
    -- whose name is @fst t@ and whose value is @snd t@.
    -- should bear
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
req type_ h q b a = maybe noProt (Right <.> useRequest) $ protocol a
  where
  noProt :: IO (Either ErrorCode (Response BS.ByteString))
  noProt = pure $ Left "req: The \"protocol\" content is Nothing."
  --
  useRequest :: Protocol -> IO (Response BS.ByteString)
  useRequest = genRequest >=> httpBS
  --
  genRequest :: Protocol -> IO Request
  genRequest p = addHeaders . addBody <$> parseRequest (prefix p ++ q)
  --
  addBody :: Request -> Request
  addBody = setRequestBodyLBS b
  --
  addHeaders :: Request -> Request
  addHeaders j = foldr (uncurry addRequestHeader) j headersToAdd
    where headersToAdd = ("Authorization", authToken' a):h
  --
  prefix :: Protocol -> String
  prefix p = type_' ++ show p ++ "://" ++ homeserver a ++ "/"
    where type_' = show type_ ++ " ";

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

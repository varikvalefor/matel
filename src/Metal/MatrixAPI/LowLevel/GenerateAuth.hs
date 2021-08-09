{-# LANGUAGE OverloadedStrings #-}

module Metal.MatrixAPI.LowLevel.GenerateAuth (
  generateAuthdRequest
) where
import Metal.Auth;
import Network.HTTP.Simple;

-- | @generateAuthdRequest@ is used to generate authorised requests for
-- the Matrix API, thereby removing some boilerplate crap.
--
-- The first argument consists of the HTTP command which should be sent,
-- a space, and the URI which is accessed.
--
-- The second argument is an 'Auth' record whose @authToken@ field must
-- be defined.
generateAuthdRequest :: String
                     -- ^ The URI of the request, including "POST",
                     -- "GET", or whatever crap is desired
                     -> Auth
                     -- ^ The user whose authorisation crap should be
                     -- added to the request
                     -> IO Request;
generateAuthdRequest r a = addHeader <$> parseRequest r
  where
  -- This "where" clause is used to avoid having a long line.
  -- Although long lines appear elsewhere within Metal's source code,
  -- the author wishes to avoid having long lines of source code iff
  -- such avoidance is feasible.
  addHeader :: Request -> Request
  addHeader = addRequestHeader "Authorization" (authToken' a);

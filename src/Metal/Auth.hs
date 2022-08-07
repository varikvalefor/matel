{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.Auth
-- Description : Metal's 'Auth' type and similar stuff
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.Auth contains Matel's 'Auth' datatype, which is used for
-- the authentication of Matel's user.
module Metal.Auth (Auth, authToken') where
import Metal.User;
import Metal.OftenUsedFunctions;
import qualified Data.Text.Lazy as T;
import qualified Data.ByteString.Lazy as BSL;
import qualified Data.ByteString.Char8 as BS8;
import qualified Data.Text.Lazy.Encoding as T;

-- | For all 'Auth' @k@, @k@ contains the authorisation information
-- of Matel's user, e.g., the username and authorisation token of the
-- user of Matel.
--
-- 'Auth' is really just a synonym of 'User'.  However, unlike most
-- instances of 'User', for all 'Auth' @k@, generally, the 'username',
-- 'authToken', and 'homeserver' fields of 'k' must be defined.
type Auth = User;

-- | @authToken' k@ is 'Maybe' a version of the authorisation token of
-- @k@ which can be used as the content of the "Authorization" header of
-- client requests.
authToken' :: User -> Maybe BS8.ByteString;
authToken' = pack . ("Bearer " ++ ) <.> authToken
  where
  pack = BSL.toStrict . T.encodeUtf8 . T.pack;

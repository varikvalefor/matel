{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @getAuthorisationDetails@; although the
-- function was previously contained within Metal.Auth, Metal makes
-- no use of @getAuthorisationDetails@.
module GetAuth (getAuthorisationDetails) where
import Metal.Auth;
import Metal.Base;
import Metal.User;
import System.Directory;
import System.Environment;
import qualified Data.Text as T;
import qualified Data.Text.IO as T;
import qualified Metal.Default as Def;

-- | @getAuthorisationDetails@ equals a 'User' value which contains
-- information which is used to authenticate Matel's user, e.g., the
-- homeserver to which requests should be sent, as well as the username
-- of Matel's user.
--
-- This authorisation-related information is read from
-- @[HOME DIRECTORY]\/.config\/matel@, whose formatting is described in
-- Matel's "README" file.
getAuthorisationDetails :: IO Auth;
getAuthorisationDetails =
  getHomeDirectory >>= T.readFile . (++ "/.config/matel") >>= \cfg ->
  return Def.user {
    username = T.unpack $ xOf "username: " cfg,
    password = xOf "password: " cfg,
    homeserver = T.unpack $ xOf "homeserver: " cfg,
    authToken = T.unpack $ xOf "authtoken: " cfg
  };

-- | @xOf a b@ equals the content of the field of @b@ whose name is @a@.
--
-- @xOf@ is used to reduce the amount of boilerplate stuff.
xOf :: Stringth -> Stringth-> Stringth;
xOf query cfg =
  T.drop n $ head $ filter ((== query) . T.take n) $ T.lines cfg
  where
  n :: Int
  n = T.length query;

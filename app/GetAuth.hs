{-# LANGUAGE OverloadedStrings #-}

-- | Module    : GetAuth
-- Description : Matel's 'Auth'-fetching crap
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- This module contains @'getAuthorisationDetails'@ and
-- @'configFilePath'@.
--
-- Although the function was previously contained within Metal.Auth,
-- Metal makes no use of @getAuthorisationDetails@.
-- @getAuthorisationDetails@ is moved to this file to ensure that the
-- complexity of Metal is minimised.
module GetAuth (getAuthorisationDetails, configFilePath) where
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
getAuthorisationDetails = fmap configToUser $ T.readFile =<< configFilePath
  where
  configToUser :: Stringth -> User
  configToUser cfg = Def.user {
    username = T.unpack $ xOf "username" cfg,
    password = xOf "password" cfg,
    homeserver = T.unpack $ xOf "homeserver" cfg,
    authToken = T.unpack $ xOf "authtoken" cfg,
    protocol = Just $ T.unpack $ xOf "protocol" cfg
  };

-- | @configFilePath@ is the path of Matel's configuration file.
configFilePath :: IO FilePath;
configFilePath = (++ "/.config/matel") <$> getHomeDirectory;

-- | @xOf a b@ equals the content of the field of @b@ whose name is @a@.
--
-- @xOf@ is used to reduce the amount of boilerplate stuff.
xOf :: Stringth
    -- ^ The name of the field whose value is returned
    -> Stringth
    -- ^ The text whose fields are searched
    -> Stringth;
xOf query' cfg = T.drop queryLen $ head $ filter isMatch $ T.lines cfg
  where
  isMatch :: T.Text -> Bool
  isMatch = (== query) . T.take queryLen
  --
  queryLen :: Int
  queryLen = T.length query
  --
  query :: Stringth
  query = T.append query' fieldSeparator
  --
  fieldSeparator :: Stringth
  fieldSeparator = ": ";

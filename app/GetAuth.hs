{-# LANGUAGE OverloadedStrings #-}

-- | Module    : GetAuth
-- Description : Matel's 'Auth'-fetching crap
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
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
import Data.Char;
import Text.Read;
import Data.Maybe;
import Metal.Auth;
import Metal.Base;
import Metal.User;
import System.Directory;
import System.Environment;
import qualified Data.Text as T;
import qualified Data.Text.IO as T;
import qualified Metal.Default as Def;

-- | @getAuthorisationDetails@ returns a 'User' value which contains
-- information which is used to authenticate Matel's user, e.g., the
-- homeserver to which requests should be sent, as well as the username
-- of Matel's user.
--
-- This authorisation-related information is read from
-- @[HOME DIRECTORY]\/.config\/matel@, whose formatting is described in
-- Matel's "README" file.
--
-- = Errors
--
-- If the Matel configuration file lacks a required field, e.g.,
-- @password@, then @getAuthorisationDetails@ throws an error and
-- halts and catches fire.
getAuthorisationDetails :: IO Auth;
getAuthorisationDetails = fmap cfgToUser $ T.readFile =<< configFilePath
  where
  -- \| "Break if missing."
  --
  -- The first argument is the name of the field, and the second
  -- argument is the value which the field 'Maybe' contains.
  bim :: String -> Maybe a -> a
  bim msg Nothing = error $ "The configuration file lacks a " ++
                            show msg ++ "field."
  bim msg (Just k) = k
  --
  cfgToUser :: Stringth -> User
  cfgToUser cfg = Def.user {
    username = bim "username" $ T.unpack <$> xOf "username" cfg,
    password = bim "password" $ xOf "password" cfg,
    homeserver = bim "homeserver" $ T.unpack <$> xOf "homeserver" cfg,
    authToken = fromMaybe "whatever" $ T.unpack <$> xOf "authtoken" cfg,
    protocol = readMaybe . map toUpper . T.unpack =<< xOf "protocol" cfg
  };

-- | @configFilePath@ is the path of Matel's configuration file.
configFilePath :: IO FilePath;
configFilePath = (++ "/.config/matel") <$> getHomeDirectory;

-- | @xOf a b@ 'Just' equals the content of the field of @b@ whose name
-- is @a@ if @b@ contains such a field.  @xOf a b@ otherwise equals
-- 'Nothing'.
--
-- A 'Maybe' value is output because some requested fields may be
-- legally absent from Matel's configuration file.  An example of
-- such a field is @authtoken@.
--
-- @xOf@ reduces the amount of boilerplate stuff.
xOf :: Stringth
    -- ^ This argument is the name of the field whose content is
    -- returned.
    -> Stringth
    -- ^ This argument is the content of the configuration file whose
    -- fields are searched.
    -> Maybe Stringth;
xOf query' = fmap (T.drop queryLen) . head' . filter isMatch . T.lines
  where
  head' = listToMaybe
  isMatch = (== query) . T.take queryLen
  queryLen = T.length query
  query = T.append query' fieldSeparator
  fieldSeparator = ": ";

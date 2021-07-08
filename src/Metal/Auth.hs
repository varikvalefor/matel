{- |
 - Module:      $Header$
 - Description: $Header$ contains Metal's "Auth" datatype and
 -              related functions.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
 -
 - $Header$ contains Matel's "Auth" datatype, which is used for
 - the authentication of Matel's user, as well as
 - getAuthorisationDetails, which fetches Matel's user's authorisation
 - information.
 - -}

{-# LANGUAGE OverloadedStrings #-}

module Metal.Auth (Auth, getAuthorisationDetails) where
import Metal.User;
import System.Environment;
import qualified Data.ByteString.Char8 as BS8;

-- | For all 'Auth' @k@, @k@ contains the authorisation information
-- of Matel's user, i.e., the username and authorisation token of the
-- user of Matel.
--
-- 'Auth' is really just a synonym of 'User'. However, unlike most
-- instances of 'User', generally, only the @username@ and @password@
-- fields need be filled.
type Auth = User;

getAuthorisationDetails :: IO Auth;
getAuthorisationDetails =
  getEnv "HOME" >>= BS8.readFile . (++ "/.config/matel") >>= \cfg ->
  return User {username = usernameOf cfg, password = passwordOf cfg};

usernameOf :: BS8.ByteString -> String;
usernameOf = BS8.unpack . xOf "username: ";

passwordOf :: BS8.ByteString -> BS8.ByteString;
passwordOf = xOf "password: ";

-- | @xOf a b@ equals the content of the field of @b@ whose name is @a@.
--
-- @xOf@ is used to reduce the amount of boilerplate stuff.
xOf :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString;
xOf query cfg =
  BS8.drop l $ head $ filter ((== query) . BS8.take l) $ BS8.lines cfg
  where
  l :: Int
  l = BS8.length query;

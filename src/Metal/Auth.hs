{-# LANGUAGE OverloadedStrings #-}

-- | Metal.Auth contains Matel's 'Auth' datatype, which is used for
-- the authentication of Matel's user, as well as
-- 'getAuthorisationDetails', which fetches Matel's user's authorisation
-- information.
module Metal.Auth (Auth, getAuthorisationDetails, authToken') where
import Metal.Base;
import Metal.User;
import System.Environment;
import qualified Data.Text as T;
import qualified Data.Text.IO as T;
import qualified Data.ByteString.Char8 as BS8;

-- | For all 'Auth' @k@, @k@ contains the authorisation information
-- of Matel's user, i.e., the username and authorisation token of the
-- user of Matel.
--
-- 'Auth' is really just a synonym of 'User'. However, unlike most
-- instances of 'User', generally, only the @username@ and @password@
-- fields need be filled.
type Auth = User;

-- | @authToken' k@ equals a version of the authorisation token of @k@
-- which can be used as the content of the "Authorization" header of
-- client requests.
authToken' :: User -> BS8.ByteString;
authToken' = BS8.pack . ("Bearer " ++ ) . authToken;

-- | @getAuthorisationDetails@ equals a 'User' value which contains
-- authorisation-related information of Matel's user, e.g, the
-- homeserver to which requests should be sent, as well as the username
-- of Matel's user.
getAuthorisationDetails :: IO Auth;
getAuthorisationDetails =
  getEnv "HOME" >>= T.readFile . (++ "/.config/matel") >>= \cfg ->
  return User {
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
  T.drop l $ head $ filter ((== query) . T.take l) $ T.lines cfg
  where
  l :: Int
  l = T.length query;

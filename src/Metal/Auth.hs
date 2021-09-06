{-# LANGUAGE OverloadedStrings #-}

-- | Metal.Auth contains Matel's 'Auth' datatype, which is used for
-- the authentication of Matel's user.
module Metal.Auth (Auth, authToken') where
import Metal.User;
import qualified Data.ByteString.Char8 as BS8;

-- | For all 'Auth' @k@, @k@ contains the authorisation information
-- of Matel's user, e.g., the username and authorisation token of the
-- user of Matel.
--
-- 'Auth' is really just a synonym of 'User'.  However, unlike most
-- instances of 'User', generally, the @username@, @authToken@, and
-- @homeserver@ fields must be defined.
type Auth = User;

-- | @authToken' k@ equals a version of the authorisation token of @k@
-- which can be used as the content of the "Authorization" header of
-- client requests.
authToken' :: User -> BS8.ByteString;
authToken' = BS8.pack . ("Bearer " ++ ) . authToken;

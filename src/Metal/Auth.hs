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

module Metal.Auth (Auth, getAuthorisationDetails) where
import Metal.User;
import System.Environment;

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
  getEnv "HOME" >>= readFile . (++ "/.config/matel") >>= \cfg ->
  return User {username = usernameOf cfg}


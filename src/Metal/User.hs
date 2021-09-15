-- | Module    : Metal.User
-- Description : Matel's representation of the Matrix user
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.User contains the 'User' datatype.
module Metal.User where
import Metal.Base;

-- | 'User' holds data regarding an arbitrary Matrix user.
data User = User {
  -- | @username k@ equals the Matrix username of @k@.
  username :: Identifier,
  -- | @password k@ equals the password of @k@.  For reasons which
  -- should be obvious, @password@ is only officially used for the
  -- authentication of Matel's user.
  password :: Stringth,
  -- | @homeserver k@ equals the FQDN of the homeserver of @k@.
  --
  -- This value should only be used to determine the FQDN of the server
  -- to which API requests should be sent; Metal does _not_ guarantee
  -- that for all 'User' @k@, @homeserver k@ is a non-default value.
  homeserver :: String,
  -- | @authToken k@ equals the authorisation token of @k@.
  --
  -- Like @'password'@, @authToken@ is used only for the user of Matel.
  authToken :: String,
  -- | @protocol k@, if present, describes the protocol which @k@ uses
  -- to connect to the Matrix homeserver of @k@.
  --
  -- The standard values are "http" and "https".
  --
  -- This value is only officially used to determine the protocol which
  -- Metal should use to contact the homeserver.  However, other uses of
  -- this thing may be possible.
  protocol :: Maybe String,
  -- | @displayname tpForMyBunghole@ equals the "display name" of
  -- @tpForMyBunghole@, e.g., "Johnny Kissass".
  displayname :: HumanReadableName
} deriving (Eq, Read, Show);

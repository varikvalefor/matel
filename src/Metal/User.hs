-- | Module    : Metal.User
-- Description : Matel's representation of the Matrix user
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.User contains the 'User' datatype.
module Metal.User where
import Metal.Base;

-- | For all 'User' @k@, @k@ holds data regarding an arbitrary Matrix
-- user.
--
-- Within this documentation, @l@ denotes the Matrix user which @k@
-- represents.
data User = User {
  -- | @username k@ equals the Matrix username of @l@.
  username :: Identifier,
  -- | @password k@ equals the password of @l@.  For reasons which
  -- should be obvious, @password@ is only officially used for the
  -- authentication of Matel's user.
  password :: Stringth,
  -- | @homeserver k@ equals the FQDN of the homeserver of @l@.
  --
  -- This value should only be used to determine the FQDN of the server
  -- to which API requests should be sent; Metal does /not/ guarantee
  -- that for all 'User' @k@, @homeserver k@ is a non-default value.
  homeserver :: String,
  -- | @authToken k@ equals the authorisation token of @l@.
  --
  -- Like @'password'@, @authToken@ is used only for the user of Matel.
  authToken :: String,
  -- | @protocol k@, if present, describes the protocol which @l@ uses
  -- to connect to the Matrix homeserver of @l@.
  --
  -- The standard values are "http" and "https".
  --
  -- This value is only officially used to determine the protocol which
  -- Metal should use to contact the homeserver.  However, other uses of
  -- this thing may be possible.
  protocol :: Maybe String,
  -- | @displayname tpForMyBunghole@ equals the "display name" of the
  -- Matrix user which @tpForMyBunghole@ represents, e.g., "Johnny
  -- Kissass".
  displayname :: HumanReadableName
} deriving (Eq, Read, Show);

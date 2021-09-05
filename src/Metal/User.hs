-- | Metal.User contains the 'User' datatype."
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
  -- | @displayname tpForMyBunghole@ equals the "display name" of
  -- @tpForMyBunghole@, e.g., "Johnny Kissass".
  displayname :: HumanReadableName
} deriving (Eq, Read, Show);

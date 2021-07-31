-- | Metal.User contains the 'User' datatype."

module Metal.User where
import Metal.Base;
import Data.Maybe (fromJust);
import Data.List (elemIndex);

-- | 'User' holds data regarding an arbitrary Matrix user.
data User = User {
  -- | For all 'User' @k@, @username k@ equals the Matrix username of
  -- @k@.
  username :: Identifier,
  -- | @password k@ equals the password of @k@.  For reasons which
  -- should be obvious, @password@ is only used for the user account of
  -- Matel's user.
  password :: Stringth,
  -- | @homeserver k@ equals the FQDN of the homeserver of @k@.
  homeserver :: String,
  -- | @authToken k@ equals the authorisation token of @k@.
  --
  -- Like @password@, @authToken@ is used only for the user of Matel.
  authToken :: String,
  -- | @displayname tpForMyBunghole@ equals the "display name" of
  -- @tpForMyBunghole@, e.g., "Johnny Kissass".
  displayname :: HumanReadableName
} deriving (Eq, Read, Show);

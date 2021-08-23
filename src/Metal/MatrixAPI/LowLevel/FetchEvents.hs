-- | This module contains @'fetchEvents'@ and some things which support
-- @'fetchEvents'@.
module Metal.MatrixAPI.LowLevel.FetchEvents (fetchEvents) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Metal.Space;
import Metal.Community;
import Network.HTTP.Simple;
import Metal.Messages.Standard;
import qualified Data.Text as T;
import qualified Data.Aeson as A;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | For all 'Event' @A@, @A@ describes a Matrix room event.
class Event a where
  -- | @fetchEvents k r a@ fetches all events of type @a@ from the room
  -- which is specified in @r@.  The authorisation information which is
  -- specified in @a@ is used to authenticate the query.
  fetchEvents :: a
              -- ^ The type of event which should be fetched
              -> Room
              -- ^ The room from which events should be fetched
              -> Auth
              -- ^ The authorisation information which is used to
              -- authenticate the query
              -> [a];

instance Event StdMess where
  fetchEvents _ _ _ = error "This fetchEvents is unimplemented.";

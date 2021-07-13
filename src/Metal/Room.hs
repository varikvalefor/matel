-- | Metal.Room contains the 'Room' datatype.

module Metal.Room where
import Metal.Base;
import Metal.User;

-- | For all 'Room' @k@, @k@ is a Matrix chatroom.
data Room = Room {
  -- | For all 'Room' @k@, @roomId k@ is the "non-human-readable"
  -- identifier of k, e.g., "!wnmjpIJcdaBNfOJrSw:matrix.org".
  roomId :: Identifier,
  -- | For all 'Room' @k@, @roomHid k@ is the "human-readable"
  -- identifier of @k@, e.g., "#johnnykissassSuckupfest:matrix.org".
  roomHumanId :: Identifier,
  -- | For all 'Room' @k@, @roomName k@ is the "human-readable" name of
  -- @k@, e.g., "Johnny Kissass's Suck-Up Fest".
  roomName :: HumanReadableName,
  -- | For all 'Room' @k@, @members k@ is the list of the members of
  -- @k@.  Matel does not sort members according to any particular
  -- thing.
  members :: [User],
  -- | For all 'Room' @k@, @topic k@ equals the topic of @k@.
  topic :: String,
  -- | @isEncrypted k@ iff encryption is enabled within @k@.
  isEncrypted :: Bool,
  -- | @publicKey k@ equals the public key of @k@.  If @k@ is
  -- unencrypted, then @publicKey k@ equals Nothing.
  publicKey :: Maybe PublicKey
} deriving (Eq, Read, Show);

-- | Metal.Space contains the 'Space' datatype.
module Metal.Space where
import Metal.Base;
import Metal.Room;
import Metal.User;

-- | For all 'Space' @k@, @k@ is a Matrix space.
data Space = Space {
  -- | @spaceId k@ equals the identifier of @k@.
  spaceId :: Identifier,
  -- | @spaceRooms k@ equals a list of all rooms which are contained
  -- within @k@.
  spaceRooms :: [Room],
  -- | @spaceMembers k@ equals a list of the members of @k@.
  spaceMembers :: [User]
} deriving (Eq, Read, Show);

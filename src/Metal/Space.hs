-- | Metal.Space contains the 'Space' datatype.

module Metal.Space where
import Metal.Base;

-- | For all 'Space' @k@, @k@ is a Matrix space.
data Space = Space {
  -- | For all 'Space' @k@, @spaceId k@ equals the identifier of @k@.
  spaceId :: Identifier
} deriving (Eq, Read, Show);

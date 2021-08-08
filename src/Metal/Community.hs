-- | Metal.Community contains the 'Community' datatype.
module Metal.Community where
import Metal.Base;

-- | For all 'Community' @k@, @k@ is a Matrix community.
--
-- Note that communities are essentially deprecated and are largely
-- replaced by 'Space's.
data Community = Community {
  -- | @commId k@ equals the community ID of @k@, e.g.,
  -- "+haskell:matrix.org".
  commId :: Identifier
} deriving (Eq, Read, Show);

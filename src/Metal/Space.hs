-- | Module    : Metal.Space
-- Description : Metal's representation of the Matrix space
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.Space contains the 'Space' datatype.
module Metal.Space where
import Metal.Base;
import Metal.Room;
import Metal.User;

-- | For all 'Space' @k@, @k@ represents a Matrix space.
--
-- Within this documentation, @l@ denotes the space which is
-- represented by @k@.
-- represents.
data Space = Space {
  -- | @spaceId k@ equals the identifier of @l@.
  spaceId :: Identifier,
  -- | @spaceRooms k@ equals a list of all rooms which are contained
  -- within @l@.
  spaceRooms :: [Room],
  -- | @spaceMembers k@ equals a list of the members of @l@.
  spaceMembers :: [User]
} deriving (Eq, Read, Show);

-- | Module    : Metal.Community
-- Description : Metal's 'Community' datatype
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.Community contains the 'Community' datatype.
module Metal.Community where
import Metal.Base;

-- | For all 'Community' @k@, @k@ represents a Matrix community.
--
-- Note that communities are essentially deprecated and are largely
-- replaced by 'Space's.
data Community = Community {
  -- | @commId k@ is the community ID of @k@, e.g.,
  -- "+haskell:matrix.org".
  commId :: Identifier
} deriving (Eq, Read, Show);

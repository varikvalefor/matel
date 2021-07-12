{- |
 - Module:      $Header$
 - Description: $Header$ contains the Space datatype.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
 -
 - $Header$ contains the Space datatype.
 - -}

module Metal.Space where
import Metal.Base;

-- | For all 'Space' @k@, @k@ is a Matrix space.
data Space = Space {
  -- | For all 'Space' @k@, @spaceId k@ equals the identifier of @k@.
  spaceId :: Identifier
} deriving (Eq, Read, Show);

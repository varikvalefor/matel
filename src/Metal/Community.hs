{- |
 - Module:      $Header$
 - Description: $Header$ contains the Community datatype.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
 -
 - $Header$ contains the Community datatype.
 - -}

module Metal.Community where
import Metal.Base;

-- | For all @'Community' k@, @k@ is a Matrix community.
data Community = Community {
  -- | For all @'Community' k@, @commId k@ equals the community ID of
  -- @k@, e.g., "+haskell:matrix.org".
  commId :: Identifier
} deriving (Eq, Read, Show);

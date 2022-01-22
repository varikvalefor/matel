-- | Module    : Metal.Messages.ThumbnailInfo
-- Description : Thumbnail image crap
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains 'ThumbnailInfo'.
module Metal.Messages.ThumbnailInfo where
-- | For all 'ThumbnailInfo' @k@, @k@ describes a thumbnail image.
data ThumbnailInfo = ThumbnailInfo {
  -- | @h k@ is the pixel-based height of the thumbnail which @k@
  -- represents.
  h :: Integer,
  -- | @w k@ is the pixel-based width of the thumbnail which @k@
  -- represents.
  w :: Integer,
  -- | @mimetype k@ is the MIME type of the thumbnail which @k@
  -- represents, e.g., @"image/bmp"@.
  mimetype :: String,
  -- | @size k@ is the byte-based size of the thumbnail which @k@
  -- describes.
  size :: Integer
} deriving (Eq, Read, Show);

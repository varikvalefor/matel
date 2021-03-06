{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.Messages.FileInfo
-- Description : File information crap
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains 'FileInfo'.
module Metal.Messages.FileInfo where
import Data.Aeson.TH;
import Metal.Messages.EncryptedFile;
import Metal.Messages.ThumbnailInfo;

-- | For all 'FileInfo' @k@, @k@ describes a file of some sort.
data FileInfo = FileInfo {
  -- | @size k@ is the byte-based length of the file which @k@
  -- describes.
  --
  -- For all 'FileInfo' @j@, let @l j@ denote the file which @j@
  -- describes.
  size :: Maybe Integer,
  -- | @mimetype k@ is the MIME type of the file which @k@ describes.
  mimetype :: Maybe String,
  -- | If @l k@ is a video file, then @duration k@ is the
  -- millisecond-based duration of @l k@.
  duration :: Maybe Integer,
  -- | If @l k@ can be displayed as an image, then @h k@ is the
  -- pixel-based height of @l k@.  @h k@ is otherwise 'Nothing'.
  h :: Maybe Integer,
  -- | If @l k@ can be displayed as an image, then @w k@ is the
  -- pixel-based width of @l k@.  @w k@ is otherwise 'Nothing'.
  w :: Maybe Integer,
  -- | If @l k@ has a thumbnail image, then @thumbnail_url k@ is the URL
  -- of the thumbnail of @l k@.  This bit is only used if the attachment
  -- is unencrypted.
  thumbnail_url :: Maybe String,
  -- | If @l k@ has a thumbnail, then @thumbnail_file k@ is the
  -- encrypted thumbnail of @l k@.  This bit is only used if the thing
  -- is encrypted.
  thumbnail_file :: Maybe EncryptedFile,
  -- | If @l k@ has a thumbnail image, then @thumbnail_info k@ contains
  -- miscellaneous information regarding the thumbnail of @l k@.
  -- @thumbnail_info k@ is otherwise 'Nothing'ness.
  thumbnail_info :: Maybe ThumbnailInfo
} deriving (Eq, Read, Show);

$(deriveJSON defaultOptions ''FileInfo);

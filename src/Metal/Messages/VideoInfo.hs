-- | This module contains 'VideoInfo'.
module Metal.Messages.VideoInfo where
import Metal.Messages.EncryptedFile;
import Metal.Messages.ThumbnailInfo;

-- | For all 'VideoInfo' @k@, @k@ contains information regarding a video
-- attachment.
data VideoInfo = VideoInfo {
  -- | @duration k@ equals the millisecond-based duration of @k@.
  duration :: Maybe Integer,
  -- | @h k@  equals the pixel-based height of @k@.
  h :: Maybe Integer,
  -- | @w k@ equals the pixel-based width of @k@.
  w :: Maybe Integer,
  -- | @mimetype k@ equals the MIME type of @k@, e.g., "@video/mp4@".
  mimetype :: Maybe String,
  -- | @size k@ is the byte-based size of @k@.
  size :: Maybe Integer,
  -- | @thumbnail_url k@ is the URL of the thumbnail of @k@.  This bit
  -- is only used if the video file is unencrypted.
  thumbnail_url :: Maybe String,
  -- | @thumbnail_file k@ is the encrypted thumbnail.  This bit is only
  -- used if the video file is encrypted.
  thumbnail_file :: Maybe EncryptedFile,
  -- | @thumbnail_info k@ contains miscellaneous information regarding
  -- the thumbnail of @k@.
  thumbnail_info :: Maybe ThumbnailInfo
} deriving (Eq, Read, Show);

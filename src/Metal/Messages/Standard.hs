-- | Module    : Metal.Messages.Standard
-- Description : Unencrypted/decrypted message type
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.Messages.Standard contains the 'StdMess' record type.
module Metal.Messages.Standard where
import Metal.Base;
import Metal.EventCommonFields;
import Metal.Messages.FileInfo;
import Metal.Messages.EncryptedFile;

-- | 'MessageType' is used to describe the types of the messages which
-- 'StdMess' records represent.  Documentation of this thing is visible
-- in the documentation of @'msgType'@; 'MessageType' is actually pretty
-- useless alone.
data MessageType = TextInnit
                 | Image
                 | Attach
                 | Sticker
                 | Notice
                 | Location
                 | Video
  deriving (Eq, Read);

instance Show MessageType where
  show k = case k of
    TextInnit -> "m.text"
    Image     -> "m.image"
    Attach    -> "m.file"
    Sticker   -> "m.sticker"
    Location  -> "m.location"
    Video     -> "m.video"
    Notice    -> "m.notice";

-- | For all 'StdMess' @k@, @k@ is an unencrypted or decrypted Matrix
-- message.  @k@ may be a standard text-based message or a message which
-- contains some sort of attachment.
data StdMess = StdMess {
  -- | @msgType k@ indicates whether @k@ is text-based or contains some
  -- sort of attachment.
  --
  -- @msgType k == TextInnit@ iff @k@ is text-based.
  --
  -- @msgType k == Image@ iff @k@ contains an embedded image.
  --
  -- @msgType k == Attach@ iff @k@ contains an attachment of some other
  -- type.
  --
  -- @msgType k == Sticker@ iff @k@ contains a "sticker".
  --
  -- @msgType k == Notice@ iff @k@ is a "notice".
  --
  -- @msgType k == Location@ iff @k@ describes a physical location of
  -- some sort, e.g., the location of the bodies.
  --
  -- @msgType k == Video@ iff @k@ contains a video clip.
  msgType :: MessageType,
  -- | @body k@ equals the unencrypted body of @k@.
  body :: MessageText,
  -- | @fmtBody k@ is the value of the "@formatted_body@" field of the
  -- JSON equivalent of @k@.
  fmtBody :: Maybe MessageText,
  -- | @fmt k@ equals the content of the "format" field of the source of
  -- @k@.
  --
  -- @fmt k == MatrixCusHTML@ iff @k@ is formatted using HTML.
  -- Per the Matrix specification as of 20210605, @fmt k@ may only equal
  -- 'MatrixCusHTML'.
  fmt :: MessageFmt,
  -- | If @'msgType' k == 'Location'@, then @geo_uri k@ is the
  -- coordinates of the location which @k@ describes.  @geo_uri k@
  -- should otherwise equal 'Nothing'.
  geo_uri :: Maybe Stringth,
  -- | If @k@ primarily serves as the container of a URL, then @url k@
  -- is this URL.
  url :: Maybe String,
  -- | If @k@ contains a file and the original filename of the file
  -- which @k@ contains is known, then @filename@ is the original
  -- filename of the file which @k@ describes.  @file k@ otherwise
  -- equals 'Nothing'.
  filename :: Maybe String,
  -- | If @msgType k == 'Attach'@ and the event which @k@ describes is
  -- originally encrypted, then @file k@ is the content of the file
  -- which @k@ describes.  @file k@ otherwise equals 'Nothing'.
  file :: Maybe EncryptedFile,
  -- | If @k@ mentions a file, then @fileInfo k@ contains some
  -- information regarding the file which @k@ describes.  @fileInfo k@
  -- otherwise equals 'Nothing'.
  fileInfo :: Maybe FileInfo,
  -- | @boilerplate k@ contains the boilerplate fields of @k@, i.e., the
  -- fields which all event types should contain.
  boilerplate :: EventCommonFields
} deriving (Eq, Read, Show);

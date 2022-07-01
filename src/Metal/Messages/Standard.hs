{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.Messages.Standard
-- Description : Unencrypted/decrypted message type
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.Messages.Standard contains the 'StdMess' record type.
module Metal.Messages.Standard where
import Data.Aeson;
import Metal.Base;
import Data.Maybe;
import Metal.EventCommonFields;
import Metal.Messages.FileInfo;
import Metal.Messages.EncryptedFile;

-- | 'MessageType' is used to describe the types of the messages which
-- 'StdMess' records represent.  Useful documentation of this thing is
-- visible in the documentation of @'msgType'@; 'MessageType' is
-- actually pretty useless alone.
--
-- For all 'MessageType' @a@, @show a@ is the Matrix API JSON enum value
-- which is equivalent to @a@.
data MessageType = TextInnit
                   -- ^ 'TextInnit' represents the Matrix API's
                   -- @m.text@.
                 | Image
                   -- ^ 'Image' represents the Matrix API's @m.image@.
                 | Attach
                   -- ^ 'Attach' represents the Matrix API's @m.file@.
                 | Sticker
                   -- ^ 'Attach' represents the Matrix API's
                   -- @m.sticker@.
                 | Notice
                   -- ^ 'Notice' represents the Matrix API's @m.notice@.
                 | Location
                   -- ^ 'Location' represents the Matrix API's
                   -- @m.location@.
                 | Video
                   -- ^ 'Video' represents the Matrix API's
                   -- @m.video@.
  deriving (Eq, Read);

instance Show MessageType where
  show = \case
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
  -- | If @k@ represents a message which is formatted using HTML, then
  -- @fmtBody k@ is 'Just' the value of the "@formatted_body@" field of
  -- the JSON equivalent of @k@.  @fmtBody k@ is otherwise 'Nothing'.
  fmtBody :: Maybe MessageText,
  -- | @fmt k@ equals the content of the "format" field of the source of
  -- @k@.
  --
  -- @fmt k == MatrixCusHTML@ iff @k@ is formatted using HTML.
  -- Per the Matrix specification as of 20210605, @fmt k@ may only equal
  -- 'MatrixCusHTML'.
  fmt :: MessageFmt,
  -- | If @'msgType' k == 'Location'@, then @geo_uri k@ is 'Just' the
  -- coordinates of the location which @k@ describes.  @geo_uri k@
  -- should otherwise equal 'Nothing'.
  geo_uri :: Maybe Stringth,
  -- | If @k@ primarily serves as the container of a URL, then @url k@
  -- is 'Just' this URL.  @url k@ is otherwise 'Nothing'.
  url :: Maybe String,
  -- | If @k@ contains a file and the original filename of the file
  -- which @k@ contains is known, then @filename@ is the original
  -- filename of the file which @k@ describes.  @filename k@ otherwise
  -- equals 'Nothing'.
  filename :: Maybe String,
  -- | If @msgType k == 'Attach'@ and the event which @k@ describes is
  -- originally encrypted, then @file k@ is 'Just' the content of the
  -- file which @k@ describes.  @file k@ otherwise equals 'Nothing'.
  file :: Maybe EncryptedFile,
  -- | If @k@ mentions a file, then @fileInfo k@ 'Just' contains some
  -- information regarding the file which @k@ describes.  @fileInfo k@
  -- otherwise equals 'Nothing'.
  fileInfo :: Maybe FileInfo,
  -- | @boilerplate k@ contains the boilerplate fields of @k@, i.e., the
  -- fields which all event types should contain.
  boilerplate :: EventCommonFields
} deriving (Eq, Show);

-- This 'ToJSON' instance is placed into this file because GHC complains
-- about "orphan instances" if this instance is placed into
-- "Metal.MatrixAPI.LowLevel.Types".
instance ToJSON StdMess where
  toJSON s = case msgType s of
    -- \| @m.notice@ messages are really just @m.text@ messages which
    -- are displayed a bit uniquely.  As such, @m.notice@ messages can
    -- be handled mostly as @m.text@ events are handled.
    m | m `elem` [TextInnit, Notice] -> object
      [
        "body" .= body s,
        "msgtype" .= show (msgType s)
      ]
    Location -> object
      [
        "body" .= body s,
        "geo_uri" .= fromMaybe (errorNoField "geo_uri") (geo_uri s),
        "msgtype" .= show (msgType s)
      ]
    Attach -> object
      [
        "body" .= body s,
        "filename" .= filename s,
        "info" .= object
        [
          "mimetype" .= maybe (errorNoField "mimetype") mimetype (fileInfo s),
          "size" .= maybe (errorNoField "size") size (fileInfo s)
        ],
        "msgtype" .= show (msgType s),
        "url" .= Metal.Messages.Standard.url s
      ]
    _unrecognised -> error $ "A proper error!  ToJSON does not account \
                             \for StdMess values of @msgType@ " ++
                             show (msgType s) ++ "."
    where
    errorNoField :: String -> a
    errorNoField j = error $ "This " ++ show (msgType s) ++
                     " lacks a " ++ show j ++ "field!";

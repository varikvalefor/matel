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
module Metal.Messages.Standard (MessageType(..), StdMess(..)) where
import Data.Aeson;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.EventCommonFields;
import Metal.Messages.FileInfo;
import Metal.Messages.EncryptedFile;
import Metal.OftenUsedFunctions (bedBathAnd);

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

-- | Where @t@ is a valid Matrix API-native representation of a Matrix
-- message type, @purse t@ is the 'MessageType' which is equivalent to
-- @t@.
purse :: String
      -- ^ This bit is the Matrix API-native representation of the type
      -- of some unencrypted @m.room.message@.
      -> MessageType;
purse t = case t of
    "m.text"     -> TextInnit
    "m.image"    -> Image
    "m.file"     -> Attach
    "m.sticker"  -> Sticker
    "m.location" -> Location
    "m.video"    -> Video
    "m.notice"   -> Notice
    _            -> error $ t ++ " is an unrecognised message type.";

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
  toJSON s = object
    [
      "content"          .= object [
        "body"           .= body s,
        "format"         .= show (fmt s),
        "formatted_body" .= fmtBody s,
        "msgtype"        .= show (msgType s),
        "filename"       .= filename s,
        "file"           .= file s,
        "info"           .= fileInfo s,
        "url"            .= Metal.Messages.Standard.url s,
        "geo_uri"        .= geo_uri s
      ],
      "event_id"         .= eventId (boilerplate s),
      "origin_server_ts" .= origin_server_ts (boilerplate s),
      "sender"           .= username (sender $ boilerplate s),
      "room_id"          .= roomId (destRoom $ boilerplate s)
    ];

-- Ditto.
instance FromJSON StdMess where
  parseJSON = withObject "StdMess" parse
    where parse t = do {
      evid <- t    .:  "event_id";
      orts <- t    .:  "origin_server_ts";
      sndr <- t    .:  "sender";
      rmid <- t    .:  "room_id";
      cunt <- t    .:  "content";
      bdhi <- cunt .:  "body";
      taip <- cunt .:  "msgtype";
      -- funk <- cunt .:? "format";
      -- \^ This bit should remain commented-out until 'msgType' is of
      -- type 'StdMess' -> 'Maybe' 'MessageFmt'; the commented-out bit
      -- works fine if 'msgType' is of type 'StdMess' -> 'Maybe'
      -- 'MessageFmt', and "@funk <- cunt@" just looks funny.
      phil <- cunt .:? "file";
      funb <- cunt .:? "formatted_body";
      info <- cunt .:? "info";
      hurl <- cunt .:? "url";
      film <- cunt .:? "filename";
      gary <- cunt .:? "geo_uri";
      return StdMess {
        body                        = bdhi,
        fmt                         = MatrixCusHTML, -- read <$> funk,
                                                     -- \^ Stet.
        fmtBody                     = funb,
        Metal.Messages.Standard.url = hurl,
        filename                    = film,
        geo_uri                     = gary,
        msgType                     = purse taip,
        file                        = phil,
        fileInfo                    = info,
        boilerplate                 = EventCommonFields {
          origin_server_ts = orts,
          eventId          = evid,
          destRoom         = Room {
            roomId      = rmid,
            roomHumanId = "",
            roomName    = Nothing,
            members     = [],
            topic       = Nothing,
            publicKey   = Nothing
          },
          sender           = User {
            username    = sndr,
            password    = "",
            homeserver  = bedBathAnd ":" sndr,
            authToken   = "",
            keyring     = Nothing,
            protocol    = Nothing,
            displayname = ""
          }
        }
      };
    };

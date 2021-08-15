{-# LANGUAGE DeriveAnyClass #-}

-- | Metal.Messages.Standard contains the 'StdMess' record type.
module Metal.Messages.Standard where
import Metal.Base;
import Metal.User;

data MessageType = TextInnit | Image | Attach | Sticker
  deriving (Eq, Read, Show);

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
  msgType :: MessageType,
  -- | @messageId k@ equals the identifier of @k@.
  messageId :: Identifier,
  -- | @body k@ equals the unencrypted body of @k@.
  body :: MessageText,
  -- | @sender k@ is the 'User'-based description of the sender of @k@.
  sender :: User,
  -- | @timestamp k@ equals the UNIX time-based timestamp of @k@,
  -- according to the origin server of @k@.
  timestamp :: UNIXTime,
  -- | @fmtBody k@ is the value of the "formatted_body" field of the
  -- JSON equivalent of @k@.
  fmtBody :: Maybe MessageText,
  -- | @fmt k@ equals the content of the "format" field of the source of
  -- @k@.
  --
  -- @fmt k == MatrixCusHTML@ iff @k@ is formatted using HTML.
  -- Per the Matrix specification as of 20210605, @fmt k@ may only equal
  -- 'MatrixCusHTML'.
  fmt :: MessageFmt,
  -- | @attachment_client k@, if present, describes the file which
  -- should be uploaded to the Matrix homeserver.  As implied by the
  -- use of the term "uploaded", @attachment_client@ should _not_ be
  -- used when fetching attachments... yet.
  --
  -- @fst <$> attachment_client k@ is the filename of the file which
  -- should be uploaded.
  --
  -- @snd <$> attachment_client k@ is the actual content of the file
  -- which should be uploaded.
  attachment_client :: Maybe (Stringth, Stringth)
} deriving (Eq, Read, Show);

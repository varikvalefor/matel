{-# LANGUAGE DuplicateRecordFields #-}

-- | Metal.MatrixAPI.LowLevel.RecordCombination contains @'combine'@
-- and some stuff which supports @'combine@'.
module Metal.MatrixAPI.LowLevel.RecordCombination (combine) where
import Data.Maybe;
import Metal.Room;
import Metal.User;
import Metal.Space;
import Metal.Community;
import Metal.Encrypted as En;
import Metal.EventCommonFields;
import Metal.Messages.FileInfo;
import Metal.Messages.ThumbnailInfo;
import Metal.Messages.Standard as S;
import Metal.Messages.EncryptedFile;
import qualified Metal.Default as Def;

-- | For all types @a@, @a@ belongs to 'Combinable' iff values of type
-- @a@ can be @'combine'@d.
class Combinable a where
  -- | Where @a@ and @b@ are values of some shared record type @c@,
  -- @combine a b@ replaces the default values of @a@ with the
  -- non-default values of @b@ and replaces the default values of @b@
  -- with the non-default values of @b@, returning the new record.
  combine :: a -> a -> a;

instance Combinable StdMess where
  combine a b = StdMess {
    msgType = g msgType,
    body = g body,
    fmtBody = g fmtBody,
    fmt = g fmt,
    attachment_client = g attachment_client,
    geo_uri = g geo_uri,
    url = g url,
    filename = g filename,
    file = g' file,
    fileInfo = g' fileInfo,
    boilerplate = combine (S.boilerplate a) (S.boilerplate b)
  } where
    g' :: Combinable b => Eq b => (StdMess -> Maybe b) -> Maybe b
    g' c = combineSingleMaybeRecord c a b
    --
    g :: Eq b => (StdMess -> b) -> b
    g c = combineSingleValue c a b Def.stdMess;

instance Combinable User where
  combine a b = User {
    username = g username,
    password = g password,
    homeserver = g homeserver,
    authToken = g authToken,
    displayname = g displayname
  } where
    g :: Eq b => (User -> b) -> b
    g c = combineSingleValue c a b Def.user;

instance Combinable Community where
  combine a b = Community {
    commId = g commId
  } where
    g :: Eq b => (Community -> b) -> b
    g c = combineSingleValue c a b Def.community;

instance Combinable Space where
  combine a b = Space {
    spaceId = g spaceId,
    spaceRooms = g spaceRooms,
    spaceMembers = g spaceMembers
  } where
    g :: Eq b => (Space -> b) -> b
    g c = combineSingleValue c a b Def.space;

instance Combinable Room where
  combine a b = Room {
    roomId = g roomId,
    roomHumanId = g roomHumanId,
    roomName = g roomName,
    members = g members,
    topic = g topic,
    isEncrypted = g isEncrypted,
    publicKey = g publicKey
  } where
    g :: Eq b => (Room -> b) -> b
    g c = combineSingleValue c a b Def.room;

instance Combinable EventCommonFields where
  combine a b = EventCommonFields {
    sender = combine (sender a) (sender b),
    origin_server_ts = g origin_server_ts,
    destRoom = combine (destRoom a) (destRoom b),
    eventId = g eventId
  } where
    g :: Eq b => (EventCommonFields -> b) -> b
    g c = combineSingleValue c a b Def.eventCommonFields;

instance Combinable FileInfo where
  combine a b = FileInfo {
    w = g w,
    h = g h,
    duration = g duration,
    mimetype = g mimetype,
    size = g size,
    thumbnail_url = g thumbnail_url,
    thumbnail_file = g' thumbnail_file,
    thumbnail_info = g thumbnail_info
  } where
    g' :: Combinable b => Eq b => (FileInfo -> Maybe b) -> Maybe b
    g' c = combineSingleMaybeRecord c a b
    --
    g :: Eq b => (FileInfo -> b) -> b
    g c = combineSingleValue c a b Def.fileInfo;

instance Combinable EncryptedFile where
  combine a b = EncryptedFile {
    url = g url,
    key = g key,
    iv = g iv,
    hashes = g hashes,
    v = g v
  } where
    g :: Eq b => (EncryptedFile -> b) -> b
    g c = combineSingleValue c a b Def.encryptedFile;

instance Combinable Encrypted where
  combine a b = Encrypted {
    ciphertext = g ciphertext,
    algorithm = g algorithm,
    device_id = g device_id,
    sender_key = g sender_key,
    session_id = g session_id,
    boilerplate = combine (En.boilerplate a) (En.boilerplate b)
  } where
    g :: Eq b => (Encrypted -> b) -> b
    g c = combineSingleValue c a b Def.encrypted;

-- | At this point, just read the source code of this function, which
-- is _very_ simple.
combineSingleValue :: Eq b
                   => (a -> b)
                   -- ^ The field constructor
                   -> a
                   -- ^ The first record whose value might be used
                   -> a
                   -- ^ The second record whose value might be used
                   -> a
                   -- ^ The default record
                   -> b;
combineSingleValue c a b d
  | c a == c b = c a
    -- Randomly determining whether the value of @c a@ or @c b@ should
    -- be used was determined.  But VARIK realised that doing such a
    -- thing safely implies using the IO monad and decreasing the
    -- simplicity of @combineSingleValue@.
    --
    -- Additionally, taking this approach would reduce the performance
    -- of @combineSingleValue@, which would be lame.
  | c a /= c d = c a
  | c b /= c d = c b
  | otherwise =  c d;

-- | @combineSingleMaybeRecord c a b@ combines 'Combinable' record
-- fields @c a@ and @c b@ on a field-by-field basis, returning the
-- result of this combination.
--
-- This thing works exclusively with fields which 'Maybe' exist.
combineSingleMaybeRecord :: Combinable b
                         => Eq b
                         => (a -> Maybe b)
                         -- ^ The field constructor
                         -> a
                         -- ^ The first record whose field may be
                         -- sacrificed or chimaera'd
                         -> a
                         -- ^ The second record whose field may be
                         -- sacrificed or chimaera'd
                         -> Maybe b
combineSingleMaybeRecord c a b
  | isNothing (c a) && isNothing (c b) = c a
  | isNothing (c a) = c b
  | isNothing (c b) = c a
  | otherwise = Just $ combine (fromJust $ c a) (fromJust $ c b);

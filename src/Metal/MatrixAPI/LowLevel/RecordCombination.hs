-- | Metal.MatrixAPI.LowLevel.RecordCombination contains @'combine'@
-- and some stuff which supports @'combine@'.
module Metal.MatrixAPI.LowLevel.RecordCombination (combine) where
import Metal.Room;
import Metal.User;
import Metal.Space;
import Metal.Community;
import Metal.Messages.FileInfo;
import Metal.Messages.Standard;
import Metal.EventCommonFields;
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
    file = g file,
    fileInfo = g fileInfo,
    boilerplate = g boilerplate
  } where
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
    sender = g sender,
    origin_server_ts = g origin_server_ts,
    destRoom = g destRoom,
    eventId = g eventId
  } where
    g :: Eq b => (EventCommonFields -> b) -> b
    g c = combineSingleValue c a b Def.eventCommonFields

instance Combinable FileInfo where
  combine a b = FileInfo {
    w = g w,
    h = g h,
    duration = g duration,
    mimetype = g mimetype,
    size = g size,
    thumbnail_url = g thumbnail_url,
    thumbnail_file = g thumbnail_file,
    thumbnail_info = g thumbnail_info
  } where
    g :: Eq b => (FileInfo -> b) -> b
    g c = combineSingleValue c a b Def.fileInfo

-- | At this point, just read the source code.
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

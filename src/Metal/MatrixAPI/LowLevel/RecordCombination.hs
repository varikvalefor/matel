-- | Metal.MatrixAPI.LowLevel.RecordCombination contains @'combine'@
-- and some stuff which supports @'combine@'.
module Metal.MatrixAPI.LowLevel.RecordCombination (combine) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.Space;
import Metal.Community;
import Metal.Messages.Standard;
import qualified Metal.Default as Def;

class Combinable a where
  -- | Where @a@ and @b@ are values of some shared record type @c@,
  -- @combine a b@ replaces the default values of @a@ with the
  -- non-default values of @b@ and replaces the default values of @b@
  -- with the non-default values of @b@, returning the new record.
  combine :: a -> a -> a;

instance Combinable StdMess where
  combine a b = StdMess {
    msgType = g msgType,
    messageId = g messageId,
    body = g body,
    sender = g sender,
    timestamp = g timestamp,
    fmtBody = g fmtBody,
    fmt = g fmt,
    attachment_client = g attachment_client
  } where
    g :: Eq b => (StdMess -> b) -> b
    g c = t c a b Def.stdMess;

instance Combinable User where
  combine a b = User {
    username = g username,
    password = g password,
    homeserver = g homeserver,
    authToken = g authToken,
    displayname = g displayname
  } where
    g :: Eq b => (User -> b) -> b
    g c = t c a b Def.user;

instance Combinable Community where
  combine a b = Community {
    commId = g commId
  } where
    g :: Eq b => (Community -> b) -> b
    g c = t c a b Def.community;

instance Combinable Space where
  combine a b = Space {
    spaceId = g spaceId,
    spaceRooms = g spaceRooms,
    spaceMembers = g spaceMembers
  } where
    g :: Eq b => (Space -> b) -> b
    g c = t c a b Def.space;

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
    g c = t c a b Def.room;

t :: Eq b
  => (a -> b)
  -- ^ The field constructor
  -> a
  -- ^ The first record whose value might be used
  -> a
  -- ^ The second record whose value might be used
  -> a
  -- ^ The default record
  -> b;
t c a b d
  | c a == c b = c a
  | c a /= c d = c a
  | c b /= c d = c b
  | otherwise =  c d;

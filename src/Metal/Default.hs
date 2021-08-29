{-# LANGUAGE OverloadedStrings #-}

-- | This module contains some default records.  These records are used
-- instead of having undefined fields because Haskell compilers
-- generally disapprove of having undefined fields in records.
module Metal.Default where
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.Space;
import Metal.Community;
import Metal.Messages.Standard;
import Metal.EventCommonFields;
import qualified Metal.Messages.VideoInfo as VI;
import qualified Metal.Messages.EncryptedFile as EF;
import qualified Metal.Messages.ThumbnailInfo as TI;

-- | @user@ is a default-valued 'User' record.
user :: User;
user = User {
  username = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  password = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  homeserver = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  authToken = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  displayname = "UNDEFINED!!!  I MAY BE AN ERROR!!!"
};

-- | @community@ is a default-valued 'Community' record.
community :: Community;
community = Community {
  commId = "UNDEFINED!!!  I MAY BE AN ERROR!!!"
};

-- | @space@ is a default-valued 'Space' record.
space :: Space;
space = Space {
  spaceId = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  spaceRooms = [room],
  spaceMembers = [user]
};

-- | @room@ is a default-valued 'Room' record.
room :: Room;
room = Room {
  roomId = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  roomHumanId = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  roomName = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  members = [user],
  topic = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  isEncrypted = False,
  publicKey = Nothing
};

-- | @stdMess@ is a default-valued 'StdMess' record.
stdMess :: StdMess;
stdMess = StdMess {
  msgType = TextInnit,
  body = "I am using incomplete software.",
  -- Insults and lame jokes are the lifeblood of Matel... and VARIK's
  -- other projects.
  fmtBody = Nothing,
  fmt = MatrixCusHTML,
  attachment_client = Just ("noods", "spaghetti\nrigatoni\nramen"),
  geo_uri = Nothing,
  url = Nothing,
  Metal.Messages.Standard.videoInfo = Nothing,
  boilerplate = eventCommonFields
};

eventCommonFields :: EventCommonFields;
eventCommonFields = EventCommonFields {
  sender = user,
  origin_server_ts = -8675309,
  eventId = "Some dummy forgot to set this value.",
  -- Above this comment is a rare instance of Matel insulting Matel's
  -- author, as opposed to Matel's user.  Observe the behaviour of
  -- this specimen... and be sure to take notes.
  destRoom = room
};

videoInfo :: VI.VideoInfo;
videoInfo = VI.VideoInfo {
  VI.duration = Nothing,
  VI.h = Nothing,
  VI.w = Nothing,
  VI.mimetype = Nothing,
  VI.size = Nothing,
  VI.thumbnail_url = Nothing,
  VI.thumbnail_file = Nothing,
  VI.thumbnail_info = Nothing
};

encryptedFile :: EF.EncryptedFile;
encryptedFile = EF.EncryptedFile {
  EF.url = "ass.varikose.god/asdfasdfasdfasdf",
  EF.key = jwk,
  EF.iv = "SHA4-65536",
  EF.hashes = [],
  EF.v = "FOR VENDETTA"
};

jwk :: EF.JWK;
jwk = EF.JWK {
  EF.kty = "oct",
  EF.key_ops = ["semaphore"],
  EF.alg = "A256CTR",
  EF.k = "tf6hZnSUTpTLCXWgKRtXddjF1KapZCJoVw1L5DMoRbpGznsKtxINNvaH9qyJn0d",
  EF.ext = True
};

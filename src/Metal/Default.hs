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

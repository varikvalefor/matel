{-# LANGUAGE OverloadedStrings #-}

-- | This module contains some default records.  These records are used
-- instead of having empty spaces because Haskell compilers generally
-- disapprove of having undefined fields in records.
module Metal.Default where
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.Space;
import Metal.Community;
import Metal.Messages.Standard;

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
  spaceRooms = [room]
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
  messageId = "Some dummy forgot to set this value.",
  body = "I am using incomplete software.",
  sender = user,
  timestamp = (-8675309),
  fmtBody = Nothing,
  fmt = MatrixCusHTML
};
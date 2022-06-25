{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.Default
-- Description : Metal's default records
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- This module contains some default records.  These records are used
-- instead of having undefined fields because record values which
-- are not fully defined can sometimes 'splode spectacularly.
module Metal.Default where
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.Space;
import Metal.Community;
import Metal.Messages.Standard;
import Metal.EventCommonFields;
import qualified Metal.Messages.Encrypted as E;
import qualified Metal.Messages.FileInfo as FI;
import qualified Metal.Messages.EncryptedFile as EF;

-- | @user@ is a default-valued 'User' record.
user :: User;
user = User {
  username = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  password = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  homeserver = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  authToken = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  displayname = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  keyring = Nothing,
  protocol = Nothing
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
  roomName = Nothing,
  members = [user],
  topic = Nothing,
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
  geo_uri = Nothing,
  url = Nothing,
  Metal.Messages.Standard.fileInfo = Nothing,
  filename = Nothing,
  file = Nothing,
  boilerplate = eventCommonFields
};

-- | @eventCommonFields@ is a default-valued 'EventCommonFields' record.
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

-- | @fileInfo@ is a default-valued 'FileInfo' record.
fileInfo :: FI.FileInfo;
fileInfo = FI.FileInfo {
  FI.duration = Nothing,
  FI.h = Nothing,
  FI.w = Nothing,
  FI.mimetype = Nothing,
  FI.size = Nothing,
  FI.thumbnail_url = Nothing,
  FI.thumbnail_file = Nothing,
  FI.thumbnail_info = Nothing
};

-- | @encryptedFile@ is a default-valued 'EF.EncryptedFile' record.
encryptedFile :: EF.EncryptedFile;
encryptedFile = EF.EncryptedFile {
  EF.url = "ass.varikose.god/asdfasdfasdfasdf",
  EF.key = jwk,
  EF.iv = "SHA4-65536",
  EF.hashes = [],
  EF.v = "FOR VENDETTA"
};

-- | @jwk@ is a default-valued 'EF.JWK' record.
jwk :: EF.JWK;
jwk = EF.JWK {
  EF.kty = "oct",
  EF.key_ops = ["semaphore"],
  EF.alg = "A256CTR",
  EF.k = "tf6hZnSUTpTLCXWgKRtXddjF1KapZCJoVw1L5DMoRbpGznsKtxINNvaH9qyJn0d",
  EF.ext = True
};

-- | @encrypted@ is a default-valued 'E.Encrypted' record.
encrypted :: E.Encrypted;
encrypted = E.Encrypted {
  E.ciphertext = "WW91J3JlIGJhZC4gIFlvdSdyZSBiY\
                 \WQuICBZb3UgZnVja2luJyBzdWNrLgo",
  E.algorithm = "AL GORE",
  E.device_id = Nothing,
  E.sender_key = "7",
  E.session_id = Nothing,
  E.boilerplate = eventCommonFields
};

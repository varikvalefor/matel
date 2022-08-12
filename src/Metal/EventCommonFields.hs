{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.EventCommonFields
-- Description : Metal's boilerplate record fields
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains 'EventCommonFields'.
--
-- SIMON MICHAEL is partially responsible for this approach to adding
-- boilerplate fields.  VARIK appreciates being slapped sensible.
module Metal.EventCommonFields (EventCommonFields(..)) where
import Data.Aeson;
import Metal.Base;
import Metal.Room;
import Metal.User;

-- | 'EventCommonFields' contains some fields which are of use to all
-- record types which represent Matrix events.
--
-- Within the following definitions, @l@ denotes the event whose record
-- representation contains this 'EventCommonFields' record.
data EventCommonFields = EventCommonFields {
  -- | @origin_server_ts k@ is the date of the creation of @l@,
  -- according to the homeserver of the user which creates @l@.
  origin_server_ts :: UNIXTime,
  -- | @sender k@ describes the user which creates @l@.
  sender :: User,
  -- | @destRoom k@ describes the Matrix room which is the destination
  -- of @l@.
  destRoom :: Room,
  -- | @eventId k@ is the event ID of @l@.
  eventId :: String
} deriving (Eq, Read, Show);

instance ToJSON EventCommonFields where
  toJSON s = object
    [
      "origin_server_ts" .= origin_server_ts s,
      "sender" .= username (sender s),
      "room_id" .= roomId (destRoom s),
      "event_id" .= eventId s
    ];

instance FromJSON EventCommonFields where
  parseJSON = withObject "EventCommonFields" parse
    -- \| Yeah, yeah, "do" notation is gross, but what is the best
    -- alternative in this case?
    where
    parse t = do {
      ost <- t .: "origin_server_ts";
      sdr <- t .: "sender";
      dst <- t .: "room_id";
      vnt <- t .: "event_id";
      return EventCommonFields {
        origin_server_ts = ost,
        sender = user {username = sdr},
        destRoom = room {roomId = dst},
        eventId = vnt
      };
    }

-- | @user@ is a default-valued 'User' record.
--
-- This thing is nabbed from "Metal.Default".  If "Metal.Default" is
-- just imported, then this module 'splodes.  The jankiness is
-- necessary.
--
-- [INSERT RETCHING HERE.]
user :: User;
user = User {
  username = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  password = Nothing,
  homeserver = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  authToken = Nothing,
  displayname = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  keyring = Nothing,
  protocol = Nothing
};

-- | @room@ is a default-valued 'Room' record.
--
-- This thing is nabbed from "Metal.Default".  If "Metal.Default" is
-- just imported, then this module 'splodes.  The jankiness is
-- necessary.
--
-- [INSERT RETCHING HERE.]
room :: Room;
room = Room {
  roomId = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  roomHumanId = "UNDEFINED!!!  I MAY BE AN ERROR!!!",
  roomName = Nothing,
  members = [user],
  topic = Nothing,
  publicKey = Nothing
};

-- | Module    : Metal.EventCommonFields
-- Description : Metal's boilerplate record fields
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains 'EventCommonFields'.
--
-- SIMON MICHAEL is partially responsible for this approach to adding
-- boilerplate fields.  VARIK appreciates being slapped sensible.
module Metal.EventCommonFields where
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

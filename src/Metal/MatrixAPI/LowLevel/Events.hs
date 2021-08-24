-- | Metal.MatrixAPI.LowLevel.Types contains the datatypes which are of
-- use only within Metal.MatrixAPI.LowLevel.
module Metal.MatrixAPI.LowLevel.Event (Room) where
import Data.Aeson;
import Metal.Base;
import Data.Aeson.TH;

-- | For all 'RoomEvent' @k@, @k@ represents a room event of some type,
-- e.g., one part of the output of the homeserver's response to a
-- "@GET /_matrix/client/r0/rooms/{roomId}/messages@" request.
data RoomEvent = RoomEvent {
  -- | @_content k@ is the value of the "@content@" field of the JSON
  -- data which is used to generate @k@.
  _content :: Content,
  -- |@_type k@ is the value of the "@type@" field of the JSON data
  -- which is used to generate @k@.
  _type :: String,
  -- | @_event_id k@ is the value of the "@event_id@ field of the JSON
  -- data which is used to generate @k@.
  _event_id :: String,
  -- | @_sender k@ is the value of the "@sender@ field of the JSON
  -- data which is used to generate @k@.
  _sender :: String,
  -- | @_origin_server_ts k@ is the value of the "@origin_server_ts@"
  -- field of the JSON data which is used to generate @k@.
  _origin_server_ts :: Integer,
  -- | @_unsigned k@ is the value of the "@unsigned@" field of the
  -- JSON data which is used to generate @k@.
  _unsigned :: UnsignedData,
  -- | @_room_id k@ is the value of the "@room_id@" field of the JSON
  -- data which is used to generate @k@.
  --
  -- This value exists within the 'Maybe' monad because this value _may_
  -- not be defined; @sync@-generated events lack this stuff.
  _room_id :: Maybe String
};

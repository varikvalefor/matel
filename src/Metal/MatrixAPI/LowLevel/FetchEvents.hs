{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'fetchEvents'@ and some things which support
-- @'fetchEvents'@.
module Metal.MatrixAPI.LowLevel.FetchEvents (fetchEvents) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Metal.Space;
import Metal.Community;
import Data.Aeson.Quick;
import Network.HTTP.Simple;
import Metal.Messages.FileInfo;
import Metal.EventCommonFields;
import Metal.Messages.Standard;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;

-- | For all 'Event' @A@, @A@ describes a Matrix room event.
class Event a where
  -- | @fetchEvents n d k r a@ fetches @n@ events of type @msgType k@
  -- from the room which is specified in @r@.  The authorisation
  -- information which is specified in @a@ is used to authenticate the
  -- query.
  --
  -- If @d == 'b'@, then the @n@ most recent messages of @k@ are
  -- returned.  If @d == 'f'@, then the @n@ earliest messages of @k@ are
  -- returned.
  fetchEvents :: Integer
              -- ^ The number of events which should be fetched
              -> Char
              -- ^ The direction of the fetching -- 'b' fetches messages
              -- which are sent recently, and 'f' fetches messages
              -- which are sent most early
              -> a
              -- ^ The type of event which should be fetched
              -> Room
              -- ^ The room from which events should be fetched
              -> Auth
              -- ^ The authorisation information which is used to
              -- authenticate the query
              -> IO [a];

instance Event StdMess where
  fetchEvents n d ms rm = process <.> TP.req TP.GET querr ""
    where
    process :: Response BS.ByteString -> [StdMess]
    process k = case getResponseStatusCode k of
      200 -> filter nonDef $ map toMessage $ (toValue k) .! "{chunk}"
      _   -> detroit k
      where
      toValue :: Response BS.ByteString -> Value
      toValue = fromMaybe chunkMissing . decode . BSL.fromStrict .
                getResponseBody
      --
      nonDef :: StdMess -> Bool
      nonDef = not . (== Def.stdMess)
      --
      chunkMissing :: a
      chunkMissing = error "Metal.MatrixAPI.LowLevel.FetchEvents.\
                     \fetchEvents: The \"chunk\" field is absent!"
    --
    toMessage :: Value -> StdMess
    toMessage k = case theMessageType of
      "m.text"     -> valueMTextToStdMess k
      "m.notice"   -> (valueMTextToStdMess k) {msgType = Notice}
      "m.image"    -> valueMImageToStdMess k
      "m.location" -> valueMLocationToStdMess k
      _            -> Def.stdMess
      where
      theMessageType :: String
      theMessageType = k .! "{content:{msgtype}}"
    --
    querr :: String
    querr = "_matrix/client/r0/rooms/" ++ roomId rm ++
            "/messages?limit=" ++ show n ++ "&filter=%7B\"types\":\
            \%5B%22m.room.message%22%5D%7D" ++
            -- \^ "Yo, only select the unencrypted stuff."
            "&dir=" ++ [d];

-- | @valueToECF k@ describes the boilerplate portion of the Matrix
-- message which @k@ represents.
valueToECF :: Value
           -- ^ A representation of the message whose boilerplate crap
           -- should be described
           -> EventCommonFields;
valueToECF k = EventCommonFields {
  sender = Def.user {username = k .! "{sender}"},
  destRoom = Def.room {roomId = k .! "{room_id}"},
  eventId = k .! "{event_id}",
  origin_server_ts = k .! "{origin_server_ts}"
};

-- | Where @k@ represents a @m.room.message@ of message type @m.text@,
-- @valueMTextToStdMess@ is a 'StdMess' which should be equivalent to
-- @k@.
valueMTextToStdMess :: Value
                    -- ^ The representation of the message which should
                    -- become a 'StdMess'
                    -> StdMess;
valueMTextToStdMess k = Def.stdMess {
  body = k .! "{content:{body}}",
  fmtBody = k .? "{content:{formatted_body}}",
  -- \^ The "formatted_body" field _should_ be
  -- present... but _may_ not be present.
  boilerplate = valueToECF k
};

-- | Where @k@ represents a @m.room.message@ of message type @m.image@,
-- @valueMTextToStdMess@ is a 'StdMess' which should be equivalent to
-- @k@.
valueMImageToStdMess :: Value -> StdMess;
valueMImageToStdMess k = Def.stdMess {
  msgType = Image,
  body = con .! "{body}",
  fileInfo = Just Def.fileInfo {
    w = con .? "{info:{w}}",
    h = con .? "{info:{h}}",
    mimetype = con .? "{info:{mimetype}}",
    size = con .? "{info:{size}}"
  },
  url = con .? "{url}",
  boilerplate = valueToECF k
} where
  con :: Value
  con = k .! "{content}";

-- | Where @k@ represents a @m.room.message@ of message type @m.location@,
-- @valueMTextToStdMess@ is a 'StdMess' which should be equivalent to
-- @k@.
valueMLocationToStdMess :: Value -> StdMess
valueMLocationToStdMess k = Def.stdMess {
  msgType = Location,
  body = k .! "{content:body}",
  geo_uri = k .! "{content:geo_uri}",
  boilerplate = valueToECF k
};

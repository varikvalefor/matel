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
  fetchEvents n d ms rm = process <.> TP.req TP.GET querr bod
    where
    process :: Response BS.ByteString -> [StdMess]
    process k = case getResponseStatusCode k of
      200 -> map toMessage $ (toValue k) .! "{chunk}"
      _   -> detroit k
      where
      toValue :: Response BS.ByteString -> Value
      toValue = fromMaybe chunkMissing . decode . BSL.fromStrict .
                getResponseBody
      --
      chunkMissing :: a
      chunkMissing = error "Metal.MatrixAPI.LowLevel.FetchEvents.\
                     \fetchEvents: The \"chunk\" field is absent!"
    --
    toMessage :: Value -> StdMess
    toMessage k = case (k .! "{content:{msgtype}}" :: String) of
      -- \^ Yes, the cheesy typecast _is_ necessary; GHC complains about
      -- type ambiguity and refuses to compile this file if this
      -- typecast is absent.
      "m.text" -> Def.stdMess {
                  body = k .! "{content:{body}}",
                  fmtBody = k .? "{content:{formatted_body}}",
                  -- \^ The "formatted_body" field _should_ be
                  -- present... but _may_ not be present.
                  boilerplate = Def.eventCommonFields {
                    sender = Def.user {username = k .! "{sender}"},
                    destRoom = Def.room {roomId = k .! "{room_id}"},
                    eventId = k .! "{event_id}",
                    origin_server_ts = k .! "{origin_server_ts}"
                  }
                }
      _        -> error "fuck"
    --
    querr :: String
    querr = "_matrix/client/r0/rooms/" ++ roomId rm ++
            "/messages?limit=" ++ show n ++ "&types=m.room.message" ++
            "&dir=" ++ [d]
    --
    bod :: BSL.ByteString
    bod = fromString $
          "{\n\t" ++
            "\"filter\": {\n\t\t" ++
              "\"types\": [\"m.room.message\"]\n\t" ++
            "}\n" ++
          "}"
    --
    fromString :: String -> BSL.ByteString
    fromString = BSL.pack . map (toEnum . fromEnum);

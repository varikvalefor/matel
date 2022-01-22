{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Module    : Metal.MatrixAPI.LowLevel.FetchEvents
-- Description : Metal's low-level event-fetching crap
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- This module contains @'fetchEvents'@ and some things which support
-- @'fetchEvents'@.
module Metal.MatrixAPI.LowLevel.FetchEvents (fetchEvents) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.Encrypted;
import Data.Aeson.Quick;
import Network.HTTP.Simple;
import Metal.Messages.FileInfo;
import Metal.EventCommonFields;
import Metal.Messages.Standard;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;

-- | For all 'Event' @A@, for all values @t@ of type @A@, @t@
-- represents a Matrix room event.
class Event a where
  -- | @nonDef a@ iff @a@ is not a default-valued thing.
  nonDef :: a
         -- ^ This record is the record whose defaultness is determined.
         -> Bool

  -- | @fetchEvents@ is used to fetch Matrix events of a specified type.
  --
  -- = Output
  --
  -- If everything goes according to plan, then a list of the desired
  -- Matrix events is 'Right'ly returned.  If just the most 'Left'-field
  -- shit happens, then a description of this happening is returned.
  fetchEvents :: Integer
              -- ^ This argument is the number of messages which should
              -- be fetched.
              -> Char
              -- ^ This bit refers to the direction in which messages
              -- should be fetched.
              --
              -- If this argument is \'b\', then @n@ events which are
              -- most recently sent should be returned.
              --
              -- If this argument is \'f\', then the @n@ events which
              -- are earliest sent should be returned.
              -> a
              -- ^ The type of this argument is the type of events which
              -- should be returned.
              -> Room
              -- ^ This argument represents the room from which events
              -- are fetched.
              -> Auth
              -- ^ This argument is the same old authorisation stuff.
              -> IO (Either ErrorCode [a]);

instance Event StdMess where
  nonDef = (/= Def.stdMess)
  fetchEvents n d ms rm = process <.> TP.req TP.GET [] querr ""
    where
    process :: Response BS.ByteString -> Either ErrorCode [StdMess]
    process k = case getResponseStatusCode k of
      200 -> extractMessages . (.! "{chunk}") <$> toValue k
      _   -> Left $ T.pack $ detroit' k
    extractMessages :: [Value] -> [StdMess]
    extractMessages = filter nonDef . map toMessage
    --
    querr :: String
    querr = "_matrix/client/r0/rooms/" ++ roomId rm ++
            "/messages?limit=" ++ show n ++ "&filter=%7B\"types\":\
            \%5B%22m.room.message%22%5D%7D" ++
            -- \^ "Yo, only select the unencrypted stuff."
            "&dir=" ++ [d];

-- | @toMessage k@ calls a function which converts @k@ into a
-- 'StdMess'.  Depending upon the \"type\" of @k@, any function of
-- various functions may be called.
toMessage :: Value -> StdMess;
toMessage k = case (k .! "{content:{msgtype}}" :: String) of
  "m.text"     -> valueMTextToStdMess k
  "m.notice"   -> (valueMTextToStdMess k) {msgType = Notice}
  "m.image"    -> valueMImageToStdMess k
  "m.location" -> valueMLocationToStdMess k
  "m.file"     -> valueMFileToStdMess k
  _            -> Def.stdMess;

-- | @valueToECF k@ describes the boilerplate portion of the Matrix
-- message which @k@ represents.
valueToECF :: Value
           -- ^ This value is a representation of the message whose
           -- boilerplate junk should be described.
           -> EventCommonFields;
valueToECF k = EventCommonFields {
  -- \| Using @(.?)@ here is /mostly/ a waste of time; the values which
  -- @valueToECF@ fetches MUST be present.
  --
  -- VARIK finds that accounting for cheesy-ass homeservers which do not
  -- adhere to Matrix's client-server API is a waste of time which
  -- probably just leads to the addition of some damn ugly source code.
  sender = Def.user {username = k .! "{sender}"},
  destRoom = Def.room {roomId = k .! "{room_id}"},
  eventId = k .! "{event_id}",
  origin_server_ts = k .! "{origin_server_ts}"
};

-- | Where @k@ represents a @m.room.message@ of message type @m.text@,
-- @valueMTextToStdMess k@ is a 'StdMess' which should be equivalent to
-- @k@.
valueMTextToStdMess :: Value
                    -- ^ This value represents the message which should
                    -- become a 'StdMess'.
                    -> StdMess;
valueMTextToStdMess k = Def.stdMess {
  body = k .! "{content:{body}}",
  fmtBody = k .? "{content:{formatted_body}}",
  -- \^ The "formatted_body" field /should/ be present... but /may/ not
  -- be present.
  boilerplate = valueToECF k
};

-- | Where @k@ represents a @m.room.message@ of message type @m.image@,
-- @valueMTextToStdMess k@ is a 'StdMess' which should be equivalent to
-- @k@.
valueMImageToStdMess :: Value
                     -- ^ The representation of the message which should
                     -- become a 'StdMess'
                     -> StdMess;
valueMImageToStdMess k = Def.stdMess {
  msgType = Image,
  body = con .! "{body}",
  fileInfo = Just Def.fileInfo {
    -- \| (Using @(.?)@ for values which absolutely should exist) at
    -- first glance seems a bit goofy.
    --
    -- However, using (.?) implies being able to handle some malformed
    -- messages /and/ not needing to manually place values into the
    -- 'Maybe' monad, which is nice.
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

-- | Where @k@ represents a @m.room.message@ of message type
-- @m.location@, @valueMTextToStdMess k@ is a 'StdMess' which should be
-- equivalent to @k@.
valueMLocationToStdMess :: Value
                        -- ^ This bit represents the message which
                        -- should be represented as a 'StdMess'.
                        -> StdMess;
valueMLocationToStdMess k = Def.stdMess {
  msgType = Location,
  body = k .! "{content:body}",
  geo_uri = k .! "{content:geo_uri}",
  boilerplate = valueToECF k
};

-- | Where @k@ represents a @m.room.message@ of message type @m.file@,
-- @valueMTextToStdMess k@ is a 'StdMess' which should be equivalent to
-- @k@.
valueMFileToStdMess :: Value
                    -- ^ This thing represents the message which should
                    -- become a 'StdMess'.
                    -> StdMess;
valueMFileToStdMess k = Def.stdMess {
  msgType = Attach,
  body = k .! "{content:{body}}",
  -- \| @filename@ should be present.  However, because @filename@ is
  -- 'Maybe'-monadic and @(.?)@ reuturns a 'Maybe'-monadic value, using
  -- @(.?)@ may be the best course of action.
  filename = k .? "{content:{filename}}",
  url = k .! "{content:{file}}"
};

instance Event Encrypted where
  nonDef = (/= Def.encrypted)
  fetchEvents n d ms rm = process <.> TP.req TP.GET [] querr ""
    where
    process :: Response BS.ByteString -> Either ErrorCode [Encrypted]
    process k = case getResponseStatusCode k of
      200 -> extractEncrypted . (.! "{chunk}") <$> toValue k
      _   -> Left $ T.pack $ detroit' k
    --
    extractEncrypted :: [Value] -> [Encrypted]
    extractEncrypted = filter nonDef . map toEncrypted
    --
    -- \| Using a "proper" @fromJSON@ thing is /possible/... but
    -- involves a relatively great amount of effort and offers no real
    -- advantage over using @(.!)@ and company.
    --
    -- @(.!)@ and company, however, are advantageous primarily because
    -- @(.!)@ and company are relatively reader-and-writer-friendly.
    toEncrypted :: Value -> Encrypted
    toEncrypted k = Def.encrypted {
      algorithm   = ct .! "{algorithm}",
      ciphertext  = ct .! "{ciphertext}",
      device_id   = ct .! "{device_id}",
      sender_key  = ct .! "{sender_key}",
      session_id  = ct .! "{session_id}",
      boilerplate = valueToECF k
    } where
      ct :: Value
      ct = k .! "{content}"
    --
    querr :: String
    querr = "_matrix/client/r0/rooms/" ++ roomId rm ++
            "/messages?limit=" ++ show n ++ "&filter=%7B\"types\":\
            \%5B%22m.room.encrypted%22%5D%7D" ++
            -- \^ "Yo, only select the unencrypted stuff."
            "&dir=" ++ [d];

-- | Where @merleHaggard@ is a 'Response' whose body contains a
-- "@chunk@" object, @toValue merleHaggard@ is 'Either' the content of
-- the "@chunk@" object of this response or an 'ErrorCode' which
-- indicates that this response lacks a "@chunk@" field for some stupid
-- reason.
toValue :: Response BS.ByteString -> Either ErrorCode Value
toValue = maybeToEither . decode . BSL.fromStrict . getResponseBody
  where
  maybeToEither :: Maybe Value -> Either ErrorCode Value
  maybeToEither = maybe (Left chunkMissing) Right
  --
  chunkMissing :: ErrorCode
  chunkMissing = "Metal.MatrixAPI.LowLevel.FetchEvents.\
                 \fetchEvents: The \"chunk\" field is absent!";

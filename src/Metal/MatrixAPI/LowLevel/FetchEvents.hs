{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Module    : Metal.MatrixAPI.LowLevel.FetchEvents
-- Description : Metal's low-level event-fetching crap
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- This module contains @'fetchEvents'@ and some things which support
-- @'fetchEvents'@.
module Metal.MatrixAPI.LowLevel.FetchEvents (fetchEvents) where
import Data.Aeson;
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Data.Aeson.Quick;
import Network.HTTP.Simple;
import Metal.Messages.Standard;
import Metal.Messages.Encrypted;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;

-- | For all 'Event' @A@, for all values @t@ of type @A@, @t@
-- represents a Matrix room event.
class Event a where
  -- | @nonDef a@ iff @a@ is not 'Just' a default-valued thing and is
  -- not 'Nothing'.
  nonDef :: Maybe a
         -- ^ This record is 'Maybe' the record whose defaultness is
         -- determined.
         -> Bool

  -- | @fetchEvents@ fetches Matrix events of a specified type.
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
              -> Room
              -- ^ This argument represents the room from which events
              -- are fetched.
              -> Auth
              -- ^ This argument is the same old authorisation stuff.
              -> IO (Either ErrorCode [a]);

instance Event StdMess where
  nonDef = maybe False (/= Def.stdMess)
  --
  fetchEvents n d rm = eit' process <.> TP.req TP.GET [] querr ""
    where
    eit' a = either (Left) a
    --
    process :: Response BS.ByteString -> Either ErrorCode [StdMess]
    process k = case getResponseStatusCode k of
      200 -> extractMessages . (.! "{chunk}") =<< toValue k
      _   -> Left $ T.pack $ detroit' k
    extractMessages :: [Value] -> Either ErrorCode [StdMess]
    extractMessages = dl . filter nonDef . map (decode . encode)
    --
    dl :: [Maybe StdMess] -> Either ErrorCode [StdMess]
    dl = toEither . sequence
    --
    toEither :: Maybe [StdMess] -> Either ErrorCode [StdMess]
    toEither  = m2e "Some StdMess event lacks the required fields."
    --
    querr :: String
    querr = "_matrix/client/r0/rooms/" ++ roomId rm ++
            "/messages?limit=" ++ show n ++ "&filter=%7B\"types\":\
            \%5B%22m.room.message%22%5D%7D" ++
            -- \^ "Yo, only select the unencrypted stuff."
            "&dir=" ++ [d];

instance Event Encrypted where
  nonDef = maybe False (/= Def.encrypted)
  --
  fetchEvents n d rm = (>>= process) <.> TP.req TP.GET [] querr ""
    where
    process :: Response BS.ByteString -> Either ErrorCode [Encrypted]
    process k = case getResponseStatusCode k of
      200 -> extractEncrypted . (.! "{chunk}") =<< toValue k
      _   -> Left $ T.pack $ detroit' k
    --
    extractEncrypted :: [Value] -> Either ErrorCode [Encrypted]
    extractEncrypted = dl . filter nonDef . map (decode . encode)
    --
    dl = toEither . sequence
    --
    toEither :: Maybe [Encrypted] -> Either ErrorCode [Encrypted]
    toEither = m2e "Some Encrypted event lacks the required fields."
    --
    querr :: String
    querr = "_matrix/client/r0/rooms/" ++ roomId rm ++
            "/messages?limit=" ++ show n ++ "&filter=%7B\"types\":\
            \%5B%22m.room.encrypted%22%5D%7D" ++
            -- \^ "Yo, only select the encrypted stuff."
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

-- | @m2e@ converts 'Maybe's into 'Either's.
--
-- @m2e t 'Nothing' == 'Right' t@.  @m2e _ ('Just' l) == 'Right' l@.
m2e :: b -> Maybe a -> Either b a;
m2e _ (Just l) = Right l;
m2e l Nothing = Left l;

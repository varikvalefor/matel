{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.MatrixAPI.LowLevel.Send
-- Description : Stuff-sending stuff
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains @'sendEvent'@ and some stuff which supports
-- @'sendEvent'@.
module Metal.MatrixAPI.LowLevel.Send (
  sendEvent
) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.Encrypted;
import Metal.FavoriteNoise;
import Network.HTTP.Simple;
import Metal.Messages.Standard;
import qualified Data.Aeson as A;
import qualified Data.ByteString as BS;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;
-- I need T.P. for my bunghole!

class Event a where
  -- | @eventType k@ is a Matrix-friendly representation of the event
  -- type of @k@, e.g., @"m.message"@.
  eventType :: a -> String;

instance Event StdMess where
  eventType a = "m.room.message";

instance Event Encrypted where
  eventType a = "m.room.encrypted";

-- | @sendEvent ev rm a@ only if @ev@ is sent to room @rm@ via @a@...
-- or an error message is returned.
sendEvent :: Event a
          => A.ToJSON a
          => a
          -- ^ The event which should be sent
          -> Room
          -- ^ The room to which the event should be sent
          -> Auth
          -- ^ The authorisation crap which is used to send the event
          -> IO (Maybe ErrorCode);
sendEvent ev rm a = qenerateQuery >>= \querr ->
                    process <$> TP.req TP.PUT [] querr (A.encode ev) a
  where
  qenerateQuery :: IO String
  qenerateQuery = (("_matrix/client/r0/rooms/" ++ roomId rm ++
                  "/send/" ++ eventType ev ++ "/") ++) <$> favoriteNoise;
  --
  process :: Response BS.ByteString -> Maybe ErrorCode
  process k = case getResponseStatusCode k of
    200 -> Nothing
    _   -> Just $ "sendEvent: " ++ responseToString k;

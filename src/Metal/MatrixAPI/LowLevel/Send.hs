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
import Network.HTTP.Simple;
import Metal.Messages.Standard;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Data.Aeson as A;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.ByteString as BS;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;
-- I need T.P. for my bunghole!

class Event a where
  -- | @eventType k@ is a Matrix-friendly representation of the event
  -- type of @k@, e.g., @"m.message"@.
  eventType :: a -> String;

instance Event StdMess where
  eventType _ = "m.room.message";

instance Event Encrypted where
  eventType _ = "m.room.encrypted";

-- | @sendEvent@ sends the specified 'Event' to the specified Matrix
-- room.
sendEvent :: Event a
          => A.ToJSON a
          => a
          -- ^ This value is the 'Event' which should be sent.
          -> Room
          -- ^ This value is a representation of the Matrix room to
          -- which the aforementioned 'Event' should be sent.
          -> Auth
          -- ^ This value is the authorisation information which is
          -- used to actually send the 'Event'.
          -> IO (Maybe ErrorCode);
sendEvent ev rm a = qenerateQuery >>= sendQuery
  where
  sendQuery ::  String -> IO (Maybe ErrorCode)
  sendQuery querr = process <$> TP.req TP.PUT [] querr (A.encode ev) a
  --
  qenerateQuery :: IO String
  qenerateQuery = (("_matrix/client/r0/rooms/" ++ roomId rm ++
                  "/send/" ++ eventType ev ++ "/") ++) <$> favoriteNoise
  --
  process :: Response BS.ByteString -> Maybe ErrorCode
  process k = case getResponseStatusCode k of
    200 -> Nothing
    _   -> Just $ "sendEvent: " `T.append` responseToStringth k;

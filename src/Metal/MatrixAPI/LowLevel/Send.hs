{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'sendEvent'@ and some stuff which supports
-- @'sendEvent'@.
module Metal.MatrixAPI.LowLevel.Send (
  sendEvent
) where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Metal.Space;
import Control.Lens;
import Metal.Community;
import Network.HTTP.Simple;
import Metal.Messages.Standard;
import qualified Data.Text as T;
import qualified Data.Aeson as A;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.Aeson.Lens as A;
import qualified Metal.Default as Def;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import Metal.MatrixAPI.LowLevel.GenerateAuth;
import Metal.MatrixAPI.LowLevel.GetRoomInformation;
import Metal.MatrixAPI.LowLevel.ResponseToWhatever;
import qualified Metal.MatrixAPI.LowLevel.HTTP as TP;
-- I need T.P. for my bunghole!

class Event a where
  -- | @eventType k@ is a Matrix-friendly representation of the event
  -- type of @k@, e.g., @"m.message"@.
  eventType :: a -> String;

instance Event StdMess where
  eventType a = "m.room.message";

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
sendEvent ev rm a = process <$> TP.req TP.PUT querr (A.encode ev) a
  where
  querr :: String
  querr = "_matrix/client/r0/rooms/" ++ roomId rm ++ "/state/" ++
          eventType ev
  --
  process :: Response BS.ByteString -> Maybe ErrorCode
  process k = case getResponseStatusCode k of
    200 -> Nothing
    _   -> Just $ "sendEvent: An error is returned.  Thus spake the \
           \homeserver: " ++ show (getResponseStatusCode k) ++ "; " ++
           show (getResponseBody k) ++ ".";

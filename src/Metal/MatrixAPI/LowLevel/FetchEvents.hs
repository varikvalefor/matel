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
import Network.HTTP.Simple;
import Metal.Messages.Standard;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Aeson as A;
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
    process k = case getResponseCode k of
      200 -> error "fetchEvents: process is unimplemented."
      _   -> detroit k
    --
    querr :: String
    querr = "_matrix/client/r0/rooms/" ++ roomId rm ++
            "/messages?limit=" ++ show n ++ "&types=" ++
            show (msgType ms) ++ "&dir=" ++ [d];

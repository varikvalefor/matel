{-# LANGUAGE OverloadedStrings #-}

-- | Metal.MatrixAPI.LowLevel.Send.Text contains @'sendTextMessage'@.
module Metal.MatrixAPI.LowLevel.Send.Text (
  sendTextMessage
) where
import Metal.Auth;
import Metal.Base;
import Metal.User;
import Text.StringRandom;
import Data.Text.Encoding;
import Network.HTTP.Simple;
import qualified Data.Text as T;
import qualified BasicPrelude as SPILL;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;
import Metal.MatrixAPI.LowLevel.GenerateAuth;

-- | @sendTextMessage a b c@ sends a message whose body is @a@ to the
-- Matrix room whose room ID is @b@.  This message is sent from the
-- Matrix account which is described in @c@.
sendTextMessage :: Stringth
                -- ^ The body of the message which should be sent
                -> Identifier
                -- ^ The internal Matrix ID of the room to which the
                -- message should be sent
                -> Auth
                -- ^ Authorisation junk
                -> IO (Maybe ErrorCode);
sendTextMessage body dest user = toMay' <$> (generateRequest >>= httpBS)
  where
  toMay' :: Response BS.ByteString -> Maybe ErrorCode
  toMay' theResp
    | getResponseStatusCode theResp== 200 = Nothing
    | otherwise = Just $ T.unpack $ responseToStringth theResp
  --
  generateRequest :: IO Request
  generateRequest =
    favoriteNoise >>= \fn ->
    setRequestBodyLBS sendreq <$> generateAuthdRequest ("PUT https://" ++ homeserver user ++ "/_matrix/client/r0/rooms/" ++ dest ++ "/send/m.room.message/" ++ fn) user
  --
  sendreq :: BSL.ByteString
  sendreq =
    "{\n\t" ++
      "\"msgtype\": \"m.text\",\n\t" ++
      "\"body\": " ++ (BSL.fromStrict $ encodeUtf8 $ SPILL.tshow body) ++ "\n" ++
    "}"
    where
    (++) :: BSL.ByteString -> BSL.ByteString -> BSL.ByteString
    (++) = BSL.append;
    -- When @-Wall@ is used, locally redefining @(++)@ makes GHC have a
    -- bit of a shit fit.  Bah.  This thing is safe.  The alternative to
    -- locally redefining @(++)@ is directly calling @BSL.append@... or,
    -- worse, creating a janky-looking alias, e.g., @(++')@.

-- | @favoriteNoise@ is a pseudorandom 'String'.
--
-- @favoriteNoise@ generates a maximum of (26+26+10)^24, which is
-- approximately equal to 10^43, pseudorandom sequences.  10^43
-- pseudorandom sequences should be sufficient.
favoriteNoise :: IO String;
favoriteNoise = T.unpack <$> stringRandomIO "[A-Za-z0-9]{24}";

-- | @responseToStringth k@ equals a 'Stringth' which describes the
-- status code of @k@.
responseToStringth :: Response a -> Stringth;
responseToStringth r = T.pack $ "Thus spake the homeserver: " ++
  show (getResponseStatusCode r) ++ ".";

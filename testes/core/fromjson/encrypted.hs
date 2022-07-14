{-# LANGUAGE OverloadedStrings #-}

-- | This test suite determines whether or not 'Encrypted'\'s 'FromJSON'
-- instance actually works.
module Main where
import Data.Bool;
import Data.Aeson;
import Metal.Room;
import Metal.User;
import System.Exit;
import Metal.EventCommonFields;
import Metal.Messages.Encrypted;
import qualified Metal.Default as Jam;
import qualified Data.ByteString.Lazy as BSL;

-- If @'decode' 'template' /= 'response'@, then 'main' returns an
-- 'ExitCode' which indicates that @'decode' 'template' /= 'response'@.
-- 'main' otherwise returns an 'ExitCode' which indicates that
-- @'decocde' 'template' == 'response'@.
main :: IO ExitCode;
main = bool exitFailure exitSuccess $ decode template == Just response;

-- | @template@ is a JSON representation of the 'Encrypted' Matrix
-- message which is validly represented by 'Encrypted'.
template :: BSL.ByteString;
template = "{\n\t\
             \\"content\": {\n\t\t\
               \\"algorithm\": \"m.megolm.v1.aes-sha2\",\n\t\t\
               \\"ciphertext\": \"ASS\",\n\t\t\
               \\"device_id\": \"SHITPIECE!\",\n\t\t\
               \\"sender_key\": \"This thing is invalid but SHOULD still be parsed.\",\n\t\t\
               \\"session_id\": \"BADGE\"\n\t\
             \},\n\t\
             \\"origin_server_ts\": 111111111,\n\t\
             \\"sender\": \"@rooseveltt:nacmeimei.varikose.god\",\n\t\
             \\"type\": \"m.room.encrypted\",\n\t\
             \\"event_id\": \"$NIGHTMAIL2-88bxM8A9\",\n\t\
             \\"room_id\": \"!reallypseudorandom:nacmeimei.varikose.god\"\n\
           \}";

-- | @response@ represents the 'Encrypted' message which is represented
-- by 'template'.
response :: Encrypted;
response = Encrypted {
             algorithm = "m.megolm.v1.aes-sha2",
             ciphertext = "ASS",
             device_id = Just "SHITPIECE!",
             sender_key = "This thing is invalid but SHOULD still be parsed.",
             session_id = Just "BADGE",
             boilerplate = EventCommonFields {
               origin_server_ts = 111111111,
               sender = User {
                 password = "",
                 authToken = "",
                 protocol = Nothing,
                 keyring = Nothing,
                 displayname = "",
                 username = "@rooseveltt:nacmeimei.varikose.god",
                 homeserver = "nacmeimei.varikose.god"
               },
               destRoom = Room {
                 roomHumanId = "",
                 roomName = Nothing,
                 members = [],
                 topic = Nothing,
                 publicKey = Nothing,
                 roomId = "!reallypseudorandom:nacmeimei.varikose.god"
               },
               eventId = "$NIGHTMAIL2-88bxM8A9"
             }
           };

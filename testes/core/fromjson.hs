{-# LANGUAGE OverloadedStrings #-}

-- | This test suite determines whether or not the 'FromJSON' instances
-- actually work.
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

-- For all JSON representations @t@, for all Metal representations
-- @k@ which are equivalent to @t@, @t == k@ iff @main@ returns an
-- 'ExitCode' which states that the "templates" really are equivalent
-- to the responses.  @main@ otherwise returns an 'ExitCode' which
-- indicates that something is 'sploded.
main :: IO ExitCode;
main = bool exitFailure exitSuccess $ and correctness
  where
  correctness = [decode tEncrypted == Just rEncrypted]

-- | @tEncrypted@ is a JSON representation of the 'Encrypted' Matrix
-- message which is validly represented by 'rEncrypted'.
tEncrypted :: BSL.ByteString;
tEncrypted = "{\n\t\
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

-- | @rEncrypted@ represents the 'Encrypted' message which is
-- represented by 'tEncrypted'.
rEncrypted :: Encrypted;
rEncrypted = Encrypted {
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

{-# LANGUAGE OverloadedStrings #-}

-- | This test suite determines whether or not the 'FromJSON' instances
-- actually work.
module Main where
import Data.Bool;
import Data.Aeson;
import Metal.Base;
import Metal.Room;
import Metal.User;
import System.Exit;
import Metal.EventCommonFields;
import Metal.Messages.Encrypted;
import Metal.Messages.EncryptedFile;
import qualified Metal.Default as Jam;
import qualified Data.ByteString.Lazy as BSL;
import qualified Metal.Messages.Standard as S;

-- For all JSON representations @t@, for all Metal representations
-- @k@ which are equivalent to @t@, @t == k@ iff @main@ returns an
-- 'ExitCode' which states that the "templates" really are equivalent
-- to the responses.  @main@ otherwise returns an 'ExitCode' which
-- indicates that something is 'sploded.
main :: IO ExitCode;
main = bool exitFailure exitSuccess $ and correctness
  where
  correctness = [decode (encode rEncrypted) == Just rEncrypted,
                 decode (encode rStdMess) == Just rStdMess]

-- | @tStdMess@ is a JSON representation of the 'S.StdMess' which is
-- validly represented by 'rStdMess'.
tStdMess :: BSL.ByteString;
tStdMess = "{\n\t\
             \\"content\": {\n\t\t\
               \\"body\": \"Bro, you stole my soup.  You are dead to me.\",\n\t\t\t\
               \\"file\": {\n\t\t\t\
                 \\"url\": \"https://media.varikose.god/bulls-hit/QuaUPjqr\",\n\t\t\t\
                 \\"hashes\": {\n\t\t\t\t\t\
                   \\"a\": \"b\"\n\t\t\t\t\
                 \},\n\t\t\t\
                 \\"iv\": \"SHA4-65536\",\n\t\t\t\
                 \\"v\": \"FOR VENDETTA\",\n\t\t\t\
                 \\"key\": {\n\t\t\t\t\
                   \\"alg\": \"A256CTR\",\n\t\t\t\t\
                   \\"ext\": true,\n\t\t\t\t\
                   \\"k\": \"tf6hZnSUTpTLCXWgKRtXddjF1KapZCJoVw1L5DMoRbpGznsKtxINNvaH9qyJn0d\",\n\t\t\t\t\
                   \\"kty\": \"oct\",\n\t\t\t\t\
                   \\"key_ops\": [\"semaphore\"]\n\t\t\t\
                 \}\n\t\t\
               \},\n\t\t\
               \\"url\": \"https://media.varikose.god/bulls-hit/QuaUPjqr\",\n\t\t\
               \\"format\": \"m.custom.html\",\n\t\t\
               \\"formatted_body\": \"<P>Bro, you stole my soup.  You are dead to me.</P><!-- Fuck you, punk. -->\",\n\t\t\
               \\"geo_uri\": \"Hell Jail\",\n\t\t\
               \\"filename\": \"goatse.avif\",\n\t\t\
               \\"msgtype\": \"m.image\"\n\t\
             \},\n\t\
             \\"sender\": \"@rooseveltt:nacmeimei.varikose.god\",\n\t\
             \\"origin_server_ts\": 111111111,\n\t\
             \\"room_id\": \"!thisjokeislame:nacmeimei.varikose.god\",\n\t\
             \\"event_id\": \"$NIGHTMAIL2-YGM8xnlP\"\n\
           \}"

-- | @rStdMess@ represents the message which is represented by
-- 'tEncrypted'.
rStdMess :: S.StdMess;
rStdMess = S.StdMess {
             S.body = "Bro, you stole my soup.  You are dead to me.",
             S.geo_uri = Just "Hell Jail",
             S.filename = Just "goatse.avif",
             S.fmt = MatrixCusHTML,
             S.fmtBody = Just "<P>Bro, you stole my soup.  You are dead\
                              \ to me.</P><!-- Fuck you, punk. -->",
             S.url = Just "https://media.varikose.god/bulls-hit/\
                          \QuaUPjqr",
             S.file = Just EncryptedFile {
               url = "https://media.varikose.god/bulls-hit/QuaUPjqr",
               hashes = [("a", "b")],
               iv = "SHA4-65536",
               v = "FOR VENDETTA",
               key = JWK {
                 alg = "A256CTR",
                 ext = True,
                 k = "tf6hZnSUTpTLCXWgKRtXddjF1KapZCJoVw1L5DMoRbpGznsKt\
                     \xINNvaH9qyJn0d",
                 kty = "oct",
                 key_ops = ["semaphore"]
               }
             },
             S.fileInfo = Nothing,
             S.msgType = S.Image,
             S.boilerplate = EventCommonFields {
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
                 roomId = "!thisjokeislame:nacmeimei.varikose.god"
               },
               eventId = "$NIGHTMAIL2-YGM8xnlP"
             }
           };

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

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

-- | @rStdMess@ is a 'StdMess' whose content is irrelevant.
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
                 password = Nothing,
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

-- | @rEncrypted@ is an 'Encrypted' message whose content is irrelevant.
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
                   password = Nothing,
                   authToken = Nothing,
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

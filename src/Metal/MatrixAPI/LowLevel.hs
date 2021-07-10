{-# LANGUAGE OverloadedStrings #-}

{- |
 - Module      :  $Header$
 - Description :  $Header$ contains "low-level" functions for Matrix.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains functions which directly access the Matrix API via
 - HTTP requests, as opposed to being abstracted.
 - -}

module Metal.MatrixAPI.LowLevel where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Data.Maybe;
import Data.Text.Encoding;
import qualified Data.Aeson as A;
import Network.HTTP.Simple;
import Data.List (elemIndex);
import Metal.Messages.Standard;
import Metal.MatrixAPI.LowLevel.Types;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | That @stillUnfinishedStayTuned@ exists implies that Matel is
-- currently useless as a Matrix client.
--
-- @stillUnfinishedStayTuned@ is removed when proper Matrix API support
-- is added to Matel.
stillUnfinishedStayTuned :: ();
stillUnfinishedStayTuned = ();

-- For all @('ByteData' z, 'PublicKey' k)@, @encryptWKey z k@ encrypts
-- @z@ with @k@, outputting the resulting ciphertext.
--
-- @encryptWKey@ is currently nonfunctional.
encryptWKey :: ByteData -> PublicKey -> CipherByteData;
encryptWKey text key = BS.pack [];

-- For all @('CipherByteData' z, 'PrivateKey' k)@, @decryptTextWKey z k@
-- decrypts @z@ with @k@, outputting the resulting 'ByteData'-based
-- data.
--
-- @decryptWKey@ is currently nonfunctional.
decryptWKey :: CipherByteData -> PrivateKey -> ByteData;
decryptWKey crip key = BS.pack [];

-- | For all 'User' @k@, if @username k@ and @password k@ are
-- set, then @login k@ fetches an authorisation token for Matrix user
-- @k@.
--
-- The 'Right' value of @loginPass k p@ equals the authorisation token
-- which results from signing in to Matrix.  The 'Left' value of
-- @loginPass k p@ exists only if an error is present... and equals a
-- description of such an error.
loginPass :: User -> IO (Either Stringth Stringth);
loginPass user =
  generateRequest >>= httpBS >>= return . responseToLeftRight
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("POST https://" ++ homeserver user ++ "/_matrix/client/r0/login") >>=
    return . setRequestBodyJSON logreq
  --
  logreq :: LoginRequest
  logreq = LoginRequest {
    lrq_type = "m.login.password",
    lrq_identifier = UserIdentifier {
      usident_type = "m.id.user",
      usident_user = username user
    },
    lrq_password = password user,
    lrq_initdispname = "Matel"
  };

-- | @sendSync@ accesses the Matrix "sync" function.
--
-- @sendSync Nothing g@ runs a parameterless "sync".
-- For all other @k@, @sendSync k g@ sends a "sync" query to Matrix such
-- that the "since" parameter of this query equals @fromJust k@.
--
-- The 'Right' value of @sendSync k g@ equals the authorisation token
-- which results from signing in to Matrix.  The 'Left' value of
-- @loginPass k g@ exists only if an error is present... and equals a
-- description of such an error.
sendSync :: Maybe String -- ^ The desired value of the query's "since" field
         -> User -- ^ The authorisation deets
         -> IO (Either Stringth Stringth);
sendSync since user =
  generateRequest >>= httpBS >>= return . responseToLeftRight
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("GET https://" ++ homeserver user ++ "/_matrix/client/r0/sync") >>=
    return . addRequestHeader "Authorization" (authToken' user) . setRequestBodyLBS syncreq
  --
  syncreq :: BSL.ByteString
  syncreq
    | isNothing since = ""
    | otherwise = fromString $ "{\"since\": \"" ++ fromJust since ++ "\"}"
  --
  fromString :: String -> BSL.ByteString
  fromString = BSL.pack . map (toEnum . fromEnum);

-- | @sendJoinedRooms k@ sends the "joined_rooms" query to the
-- homeserver of @k@, authenticating as @k@.
--
-- The 'Right' value of @sendJoinedRooms k g@ equals the authorisation
-- token which results from signing in to Matrix.  The 'Left' value of
-- @sendJoinedRooms k@ exists only if an error is present... and equals a
-- description of such an error.
sendJoinedRooms :: User -> IO (Either Stringth ListOfRoomIdentifiers);
sendJoinedRooms a =
  generateRequest >>= httpBS >>= return . responseToLeftRight
  where
  generateRequest :: IO Request
  generateRequest =
    parseRequest ("GET https://" ++ homeserver a ++ "/_matrix/client/r0/joined_rooms") >>=
    return .addRequestHeader "Authorization" (authToken' a)
  --
  fromString :: String -> BSL.ByteString
  fromString = BSL.pack . map (toEnum . fromEnum);

-- | @getRoomInformation room a@ equals a 'Room'-based representation of
-- the Matrix room whose internal Matrix ID is @room@ if the "yo, what
-- the hell is this thing" HTTP query works.
-- @getRoomInformation room a@ otherwise equals a description of the
-- problem which is encountered when the "describe this shiznit" query
-- is sent to the Matrix homeserver.
getRoomInformation :: Identifier -> Auth -> IO (Either Stringth Room);
getRoomInformation rommel mcdommel =
  error "getRoomInformation is unimplemented.";

-- | If the response code of @k@ equals @200@, then
-- @responseToLeftRight k@ equals the response body of @k@.
-- @responseToLeftRight k@ otherwise equals a string which contains the
-- status code of @k@.
responseToLeftRight :: Response BS.ByteString -> Either BS.ByteString BS.ByteString;
responseToLeftRight k
  | getResponseStatusCode k == 200 =
    Right $ getResponseBody k
  | otherwise =
    Left $ fromString $ "Thus spake the homeserver: " ++
    (show $ getResponseStatusCode k) ++ "."
  where
  fromString :: String -> Stringth
  fromString = BS.pack . map (toEnum . fromEnum);

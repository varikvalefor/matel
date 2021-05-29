{- |
 - Module      :  $Header$
 - Description :  $Header$ contains Metal's data types and records.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains the data types and records of Metal.
 - -}
 
module Metal.Base where
import Brick;
import qualified Data.ByteString as BS;

-- DATATYPES -----------------------------------------------------------

-- | For all Identifier k, k is a "non-human-readable" name, e.g.,
-- "@johnnykissass:matrix.varikose.god".
type Identifier = BS.ByteString;
-- | HumanReadableName x is a "human-readable" name, e.g., "Asshat", as
-- opposed to "@asshat:matrix.varikose.god".
type HumanReadableName = BS.ByteString;
-- | Unencrypted/de-encrypted text-based message data is of type
-- MessageText.  This comment almost fit on one line.  #SAD!
type MessageText = BS.ByteString;
-- | Screen is equivalent to Brick's Widget ().  Screen x is TUI data
-- which should be immediately drawn.
type Screen = Widget ();
-- | Winda is equivalent to Brick's Widget ().  Winda is used to
-- represent widgets which should NOT be immediately drawn.  Windas need
-- additional processing.
type Winda = Widget ();
-- | For all users k, k is represented as a 2-tuple User l, with fst l
-- representing the @username:homeserver.whatevs-based identifier of k
-- and snd l representing the human-readable name of k, e.g., "Asshat".
type User = (Identifier, HumanReadableName);

-- ROOM RECORDS --------------------------------------------------------

data Room = Room {
  -- | For all Room k, roomId k is the "non-human-readable" name of k,
  -- e.g., "#johnnykissassSuckupfest:matrix.varikose.god".
  roomId :: Identifier
} deriving (Eq, Read, Show);

-- MESSAGE RECORDS -----------------------------------------------------

-- | For all TextMess k, k is an unencrypted or de-encrypted Matrix
-- message.
data TextMess = TextMess {
  -- | For all TextMess k, body k contains the unencrypted body of k.
  body :: MessageText,
  -- | For all TextMess k, sender k is the User-based description of the
  -- sender of k.
  sender :: User
} deriving (Eq, Read, Show);

-- | For all CryptoMess k, k is an encrypted Matrix message.
data CryptoMess = CryptoMess {
  -- | The encryptedContent field contains the encrypted message.
  encryptedContent :: BS.ByteString
} deriving (Eq, Read, Show);

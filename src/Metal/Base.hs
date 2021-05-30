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
-- "@johnnykissass:matrix.varikose.god", as opposed to "Johnny Kissass".
type Identifier = BS.ByteString;
-- | For all HumanReadableName x, x is a "human-readable" name, e.g.,
-- "Asshat", as opposed to "@asshat:matrix.varikose.god".
type HumanReadableName = BS.ByteString;
-- | Unencrypted/de-encrypted text-based message data is of type
-- MessageText.  This comment almost fit on one line.  #SAD!
type MessageText = BS.ByteString;
-- | Screen is equivalent to Brick's Widget ().
-- For all Screen x, x is TUI data which should be immediately drawn.
type Screen = Widget ();
-- | Winda is equivalent to Brick's Widget ().
-- For all Winda k, k is a Widget which should NOT be immediately drawn
-- to the terminal; k requires additional processing.
type Winda = Widget ();
-- | For all users k, k is represented as a 2-tuple User l, with fst l
-- representing the @username:homeserver.whatevs-based identifier of k
-- and snd l representing the human-readable name of k, e.g., "Asshat".
type User = (Identifier, HumanReadableName);
-- | For all UNIXTime k, k is a seconds-since-the-UNIX-epoch-based
-- timestamp.
type UNIXTime = Integer;

-- ROOM RECORDS --------------------------------------------------------

-- | For all Room k, k is a Matrix chatroom.
data Room = Room {
  -- | For all Room k, roomId k is the "non-human-readable" name of k,
  -- e.g., "#johnnykissassSuckupfest:matrix.varikose.god".
  roomId :: Identifier,
  -- | For all Room k, roomName k is the "human-readable" name of k,
  -- e.g., "Johnny Kissass's Suck-Up Fest".
  roomName :: HumanReadableName
} deriving (Eq, Read, Show);

-- MESSAGE RECORDS -----------------------------------------------------

-- | For all TextMess k, k is an unencrypted or de-encrypted Matrix
-- message.
data TextMess = TextMess {
  -- | For all TextMess k, body k contains the unencrypted body of k.
  body :: MessageText,
  -- | For all TextMess k, sender k is the User-based description of the
  -- sender of k.
  sender :: User,
  -- | For all TextMess k, timestamp k equals the UNIX time-based
  -- timestamp of k, according to the origin server of k.
  timestamp :: UNIXTime
} deriving (Eq, Read, Show);

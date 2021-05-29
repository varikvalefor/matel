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

type Identifier = BS.ByteString;
type HumanReadableName = BS.ByteString;
type MessageText = BS.ByteString;
type Screen = Widget ();
type Winda = Widget ();
type User = (Identifier, HumanReadableName);

-- ROOM RECORDS --------------------------------------------------------

data Room = Room {
  roomId :: Identifier
} deriving (Eq, Read, Show);

-- MESSAGE RECORDS -----------------------------------------------------

data TextMess = TextMess {
  body :: MessageText,
  sender :: User
} deriving (Eq, Read, Show);
{- TextMess contains the non-encrypted content of a message. -}

data CryptoMess = CryptoMess {
  encryptedContent :: BS.ByteString
} deriving (Eq, Read, Show);
{- CryptoMess is an encrypted message. -}

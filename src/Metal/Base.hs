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

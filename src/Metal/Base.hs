module Metal.Base where
import Brick;

type Identifier = String;
type HumanReadableName = String;
type Screen = Widget ();
type Winda = Widget ();

data Room = Room {
  roomId :: Identifier
} deriving (Eq, Read, Show);

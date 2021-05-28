module Metal.Data where
import Metal.Types;

data Room = Room {
  roomId :: Identifier
} deriving (Eq, Read, Show);

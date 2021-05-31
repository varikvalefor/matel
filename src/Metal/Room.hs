{- |
 - Module      :  $Header$
 - Description :  $Header$ contains the Room datatype.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains the Room datatype.
 - -}
 
module Metal.Room where
import Metal.Base;
import Brick;

-- | For all Room k, k is a Matrix chatroom.
data Room = Room {
  -- | For all Room k, roomId k is the "non-human-readable" name of k,
  -- e.g., "#johnnykissassSuckupfest:matrix.varikose.god".
  roomId :: Identifier,
  -- | For all Room k, roomName k is the "human-readable" name of k,
  -- e.g., "Johnny Kissass's Suck-Up Fest".
  roomName :: HumanReadableName
} deriving (Eq, Read, Show);

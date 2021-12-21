-- | Module    : Metal.Room
-- Description : Matel's representation of the Matrix room
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.Room contains the 'Room' datatype.
module Metal.Room where
import Metal.Base;
import Metal.User;

-- | For all 'Room' @k@, @k@ is a Matrix chatroom.
data Room = Room {
  -- | @roomId k@ is the "non-human-readable" identifier of @k@, e.g.,
  -- "!wnmjpIJcdaBNfOJrSw:matrix.org".
  roomId :: Identifier,
  -- | @roomHumanId k@ is the "human-readable" identifier of @k@, e.g.,
  -- "#johnnykissassSuckupfest:matrix.org".
  roomHumanId :: Identifier,
  -- | @roomName k@ is the display name of @k@, e.g., "Johnny Kissass's
  -- Suck-Up Fest".
  roomName :: Stringth,
  -- | @members k@ is a list of the members of @k@.  Matel does not
  -- sort members according to any particular thing.
  members :: [User],
  -- | @topic k@ equals the topic of @k@.
  topic :: Stringth,
  -- | @isEncrypted k@ iff encryption is enabled within @k@.
  isEncrypted :: Bool,
  -- | @publicKey k@ equals the public key of @k@.  If @k@ is
  -- unencrypted, then @publicKey k@ equals Nothing.
  publicKey :: Maybe PublicKey
} deriving (Eq, Read, Show);

-- | Module    : Metal.Room
-- Description : Matel's representation of the Matrix room
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.Room contains the 'Room' datatype.
module Metal.Room where
import Metal.Base;
import Metal.User;

-- | For all 'Room' @k@, @k@ represents a Matrix chatroom.
--
-- Within this documentation, let @l@ denote the Matrix room which @k@
-- represents.
data Room = Room {
  -- | @roomId k@ is the "non-human-readable" identifier of @l@, e.g.,
  -- "!wnmjpIJcdaBNfOJrSw:matrix.org".
  roomId :: Identifier,
  -- | @roomHumanId k@ is the "human-readable" identifier of @l@, e.g.,
  -- "#johnnykissassSuckupfest:matrix.org".
  roomHumanId :: Identifier,
  -- | If @l@ has a display name, then @roomName k@ is 'Just' the
  -- display name of @l@, e.g., "Johnny Kissass's Suck-Up Fest".
  -- @roomName k@ is otherwise 'Nothing'.
  roomName :: Maybe Stringth,
  -- | @members k@ is a list of the members of @l@.  Matel does not
  -- sort members according to any particular thing.
  members :: [User],
  -- | If @l@ has a topic, then @topic k@ 'Just' equals the topic of
  -- @l@.  @topic k@ is otherwise 'Nothing'.
  topic :: Maybe Stringth,
  -- | If @l@ is encrypted, then @publicKey k@ 'Just' equals the public
  -- key of @l@.  @publicKey k@ is otherwise 'Nothing'.
  publicKey :: Maybe PublicKey
} deriving (Eq, Read, Show);

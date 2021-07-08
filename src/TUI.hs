{- |
 - Module:      $Header$
 - Description: $Header$ contains the TUI-related bits of Matel.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
 -
 - $Header$ contains some functions which Matel uses when drawing stuff
 - to the terminal.
 - Stuff is placed into $Header$ to ensure that Matel's source code is
 - at least somewhat organised.
 - -}

module TUI where
import Brick;
import Brick.Widgets.Center;
import Brick.Widgets.Border;
import Brick.Widgets.Border.Style;
import Control.Concurrent;
import Control.Monad;
import Metal.Base;
import Metal.Room;
import Metal.Messages.Standard;

-- | 'Screen' is equivalent to Brick's 'Widget ()'.
--
-- For all @'Screen' x@, @x@ is TUI data which should be immediately
-- drawn.
type Screen = Widget ();
-- | 'Winda' is equivalent to Brick's 'Widget ()'.
--
-- For all @'Winda' k@, @k@ is a 'Widget' which should NOT be
-- immediately drawn to the terminal; @k@ requires additional
-- processing.
type Winda = Widget ();

-- | For all @'MVar' 'Winda' k@, @summonTUI k@ converts the terminal
-- into a Matel TUI which receives data from @k@.
summonTUI :: MVar Winda -> IO ();
summonTUI = takeMVar >=> simpleMain . toScreen;

-- | For all @'Winda' g@, @toScreen g@ equals a 'Screen' which
-- accurately represents the content of @g@.
--
-- @toScreen k@ currently just equals a bordered @k@.
toScreen :: Winda -> Screen;
toScreen = withBorderStyle unicode . borderWithLabel (str "Matel");

-- | @temporaryMessage@ is a 'Winda' which is used to state that Matel
-- is currently pretty useless.
--
-- @temporaryMessage@ is removed only if Matel functions as intended.
temporaryMessage :: Winda;
temporaryMessage = center $ str $
  "Matel is unfinished -- check back later.\n" ++
  "Alternatively, contribute to the project!";

-- | For all @(['Room'] r, ['StdMess'] t, ['User'] x)@,
-- @dataToWinda r t x@ equals a 'Winda' which displays @r@, @t@, and
-- @x@.
--
-- @dataToWinda@ is currently unimplemented.
dataToWinda :: [Room] -- ^ List of joined rooms
            -> [StdMess] -- ^ List of recent messages in current room
            -> [User] -- ^ List of members of current room
            -> Winda;
dataToWinda r t u = center $ str "dataToWinda is unimplemented.";

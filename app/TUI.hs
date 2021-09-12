-- | Module    : TUI
-- Description : Matel's main TUI crap
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : POSIX
--
-- TUI contains some functions which Matel uses when drawing stuff to
-- the terminal.
--
-- Stuff is placed into TUI to ensure that Matel's source code is at
-- least somewhat organised.
module TUI where
import Brick;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Control.Monad;
import Control.Concurrent;
import Brick.Widgets.Center;
import Brick.Widgets.Border;
import Metal.Messages.Standard;
import Brick.Widgets.Border.Style;

-- | 'Screen's are TUI data which should be immediately drawn.
type Screen = Widget ();

-- | For all 'Winda' @k@, @k@ is a 'Widget' which should NOT be
-- immediately drawn to the terminal; @k@ requires additional
-- processing.
type Winda = Widget ();

-- | @summonTUI k@ converts the terminal into a Matel TUI which receives
-- and prints data which is fetched from @k@.
summonTUI :: MVar Winda
          -- ^ The 'MVar' which describes the 'Winda' to which the
          -- collected data should be drawn
          -> IO ();
summonTUI = takeMVar >=> simpleMain . toScreen;

-- | @toScreen g@ equals a 'Screen' which accurately represents the
-- content of @g@.
--
-- @toScreen k@ is currently just a bordered @k@.
toScreen :: Winda
         -- ^ The 'Winda' which should be converted into a 'Screen'
         -> Screen;
toScreen = withBorderStyle unicode . borderWithLabel (str "Matel");

-- | @temporaryMessage@ is a 'Winda' which is used to state that Matel
-- is currently pretty useless.
--
-- @temporaryMessage@ is removed only if Matel functions as intended.
temporaryMessage :: Winda;
temporaryMessage = center $ str $
  "Matel is unfinished -- check back later.\n" ++
  "Alternatively, contribute to the project!";

-- | @dataToWinda r t x@ equals a 'Winda' which displays @r@, @t@, and
-- @x@.
--
-- @dataToWinda@ is currently unimplemented.
dataToWinda :: [Room]
            -- ^ List of joined rooms
            -> [StdMess]
            -- ^ List of recent messages in current room
            -> [User]
            -- ^ List of members of current room
            -> Winda;
dataToWinda r m u = center $ str "dataToWinda is unimplemented.";

{- |
 - Module      :  $Header$
 - Description :  $Header$ contains the GUI-related bits of Matel.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains some functions which Matel uses when drawing stuff
 - to the terminal.
 - Stuff is placed into this file to ensure that Matel's source code is
 - at least somewhat organised.
 - -}

module GUI where
import Brick;
import Brick.Widgets.Center;
import Brick.Widgets.Border;
import Brick.Widgets.Border.Style;
import Control.Concurrent;
import Control.Monad;
import Metal.Base;

summonTUI :: MVar Winda -> IO ();
summonTUI = takeMVar >=> simpleMain . toWinda;

toWinda :: Winda -> Screen;
toWinda newData = 
  withBorderStyle unicode $
  borderWithLabel (str "Matel") newData;

temporaryMessage :: Winda;
temporaryMessage = center $ str
  "Matel is unfinished -- check back later.\nAlternatively, contribute to the project!";

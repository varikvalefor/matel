module GUI where
import Brick;
import Brick.Widgets.Center;
import Brick.Widgets.Border;
import Brick.Widgets.Border.Style;
import Control.Concurrent;
import Control.Monad;
import Metal.Data;
import Metal.Types;

summonTUI :: MVar Winda -> IO ();
summonTUI = takeMVar >=> simpleMain . toWinda;

toWinda :: Winda -> Screen;
toWinda newData = 
  withBorderStyle unicode $
  borderWithLabel (str "Matel") newData;

temporaryMessage :: Winda;
temporaryMessage = center $ str
  "Matel is unfinished -- check back later.\nAlternatively, contribute to the project!";

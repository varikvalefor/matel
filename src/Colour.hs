{- |
 - Module      :  $Header$
 - Description :  $Header$ contains the colour-related bits of Matel.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains some predefined colours which Matel uses when Matel
 - draws Matel's TUI.
 - Matel is designed to work best with light themes; as such, the
 - default colour scheme may not work well with all terminals.  Luckily,
 - VARIK, being a cool guy, writes the Colour module such that modifying
 - Matel's colour scheme should be pretty easy.
 - -}

module Colour where
import Brick.Util (fg);
import Graphics.Vty.Attributes (Attr);
import Graphics.Vty.Attributes.Color;

-- MESSAGES ------------------------------------------------------------
-- | messHight is the colour of the highlighted message, although
-- the meaning of such highlighting is yet to be determined.
messHight :: Attr;
messHight = fg red;
-- | messStd is the default message text colour.
messStd :: Attr;
messStd = fg black;

-- HOWIE MANDEL... NO, HANDLES -----------------------------------------
-- | usernameClient is the colour in which the username of Matel's user
-- should be written.
usernameClient :: Attr;
usernameClient = fg $ rgbColor 0 64 0;

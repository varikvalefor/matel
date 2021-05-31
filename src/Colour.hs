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
 -
 - The current design of Colour is extremely unstable and shall most
 - likely be eventually tossed in favour of a relatively "clean"
 - solution, e.g., actually using Brick to Brick's full potential.
 - -}

module Colour where
-- | RGBSubValue is an integer value.  For all RGBSubValue k,
-- 0 <= k <= 255.  Until a means of ensuring such bounds is implemented,
-- attempting to blow up Matel is not strongly advised.  ;^)
type RGBSubValue = Int;
-- | RGBValue is a 3-tuple of a red value, a green value, and a blue
-- value.
type RGBValue = (RGBSubValue, RGBSubValue, RGBSubValue);

-- MESSAGES ------------------------------------------------------------
-- | messHight is the colour of the highlighted message, although
-- the meaning of such highlighting is yet to be determined.
messHight :: RGBValue;
messHight = (255, 0, 0);
-- | messStd is the default message text colour.
messStd :: RGBValue;
messStd = (0, 0, 0);

-- HOWIE MANDEL... NO, HANDLES -----------------------------------------
-- | usernameClient is the colour in which the username of Matel's user
-- should be written.
usernameClient :: RGBValue;
usernameClient = (0, 64, 0);

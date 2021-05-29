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
 - -}

module Colour where
type RGBSubValue = Int;
type RGBValue = (RGBSubValue, RGBSubValue, RGBSubValue);
{- RGBValue is a 3-tuple of a red value, a green value, and a blue
 - value.  Each such value's minimum and maximum are 0 and 255,
 - respectively. -}
{- Matel is designed to work best with light themes; as such, the
 - default colour scheme may not work well with all terminals.  Luckily,
 - VARIK, being a cool guy, writes the Colour module such that modifying
 - Matel's colours should be pretty easy. -}

-- MESSAGES ------------------------------------------------------------
messHighlight :: RGBValue;
messHighlight = (255, 0, 0);

messStd :: RGBValue;
messStd = (0, 0, 0);

-- HOWIE MANDEL... NO, HANDLES -----------------------------------------
usernameClient :: RGBValue;
usernameClient = (0, 64, 0);

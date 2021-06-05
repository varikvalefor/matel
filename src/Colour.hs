{- |
 - Module:      $Header$
 - Description: $Header$ contains the colour-related bits of Matel.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -              
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
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

-- | messHight describes the formatting of the highlighted message,
-- although the meaning of such highlighting is yet to be determined.
messHight :: Attr;
messHight = fg red;

-- | messStd describes the formatting of the default message.
messStd :: Attr;
messStd = fg black;

-- HOWIE MANDEL... NO, HANDLES -----------------------------------------

-- | usernameClient describes the formatting of the username of Matel's
-- user.
usernameClient :: Attr;
usernameClient = fg $ rgbColor 0 64 0;

-- | usernameMod describes the formatting of the usernames of chatrooms'
-- moderators.
usernameMod :: Attr;
usernameMod = fg $ rgbColor 64 0 0;

-- | usernameAdmin describes the formatting of the usernames of
-- chatrooms' admins.
usernameAdmin :: Attr;
usernameAdmin = fg $ rgbColor 128 0 0;

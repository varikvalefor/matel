-- | Module    : Colour
-- Description : Matel's colour-coding crap
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : POSIX
--
-- This module contains some predefined colours which Matel uses when
-- Matel draws Matel's TUI.
--
-- Matel is designed to work best with light themes; as such, the
-- default colour scheme may not work well with all terminals.  Luckily,
-- VARIK, being a cool guy, writes this module such that modifying
-- Matel's colour scheme should be pretty easy.
module Colour (
  -- * Messages
  --
  -- $messages
  messHigh,
  messStd,
  messYcode,
  -- * HOWIE MANDEL... No, Handles
  --
  -- $fat
  usernameClient,
  usernameMod,
  usernameAdmin
) where
import Brick.Util (fg);
import Graphics.Vty.Attributes (Attr);
import Graphics.Vty.Attributes.Color;

-- $messages
--
-- This section contains some 'Attr' values which are used to colour
-- the content of messages.

-- | @messHigh@ describes the formatting of the highlighted message,
-- although the meaning of such highlighting is yet to be determined.
messHigh :: Attr;
messHigh = fg red;

-- | @messStd@ describes the formatting of the default message.
messStd :: Attr;
messStd = fg black;

-- | @messYcode@ describes the formatting of code blocks, i.e., the
-- the things what are written between backticks and may or may not
-- contain source code.
messYcode :: Attr;
messYcode = fg $ rgbColor 0 0 64;

-- $fat
--
-- This section contains some 'Attr' values which are used to colour
-- the usernames of Matrix users.

-- | @usernameClient@ describes the formatting of the username of
-- Matel's user.
usernameClient :: Attr;
usernameClient = fg $ rgbColor 0 64 0;

-- | @usernameMod@ describes the formatting of the usernames of
-- chatrooms' moderators.
usernameMod :: Attr;
usernameMod = fg $ rgbColor 64 0 0;

-- | @usernameAdmin@ describes the formatting of the usernames of
-- chatrooms' admins.
usernameAdmin :: Attr;
usernameAdmin = fg $ rgbColor 128 0 0;

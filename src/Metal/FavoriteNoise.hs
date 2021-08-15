{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'favoriteNoise'@.  @'favoriteNoise'@ is moved
-- to this module to ensure that multiple functions can take advantage
-- of the existence of @'favoriteNoise'@.
module Metal.FavoriteNoise where
import Metal.Base;
import Text.StringRandom;
import qualified Data.Text as T;

-- | @favoriteNoise@ is a pseudorandom 'String' which matches the
-- regular expression @[A-Za-z0-9]{24}@.
--
-- @favoriteNoise@ generates a maximum of (26+26+10)^24, which is
-- approximately equal to 10^43, pseudorandom sequences.  10^43
-- pseudorandom sequences should be sufficient.
favoriteNoise :: IO String;
favoriteNoise = T.unpack <$> stringRandomIO "[A-Za-z0-9]{24}";

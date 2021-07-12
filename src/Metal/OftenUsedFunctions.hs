{- |
 - Module:      $Header$
 - Description: $Header$ contains some boilerplate functions.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
 -
 - $Header$ contains some functions which are used by numerous modules
 - of Matel.
 - -}

module Metal.OftenUsedFunctions where
import Data.Either;

-- | @justLeft (Left k) == k@.  @justLeft (Right g)@ throws an error.
--
-- @justLeft@ is used because VARIK does not wish to add junk data to
-- @fromLeft@ statements, for VARIK finds that such junk data looks a
-- bit inelegant, unlike this perfectly-wrapped paragraph.
--
-- Compare with @justRight@.
justLeft :: Either a b -> a;
justLeft (Left a) = a;
justLeft (Right b) = error "justLeft is applied to a value of type 'Right'!";

-- | @justRight (Right k) == k@.  @justRight (Left g)@ throws an error.
--
-- @justRight@ is used because VARIK does not wish to add junk data to
-- @fromRight@ statements, for VARIK finds that such junk data looks a
-- bit inelegant, unlike this perfectly-wrapped paragraph.
--
-- Compare with @justLeft@.
justRight :: Either a b -> b;
justRight (Right b) = b;
justRight (Left a) = error "justRight is applied to a value of type 'Left'!";

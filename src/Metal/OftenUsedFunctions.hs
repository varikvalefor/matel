{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.OftenUsedFunctions
-- Description : Miscellaneous boilerplate functions
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.OftenUsedFunctions contains some functions which are used by
-- numerous modules of Metal.
module Metal.OftenUsedFunctions where
import Metal.Base;
import Text.StringRandom;
import Network.HTTP.Simple;
import qualified Data.Text as T;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | @detroit k@ throws an @'error'@ such that a description of @k@ is
-- written to the standard error.
--
-- To ensure that a decent amount of debug information is available if
-- stuff breaks, where appropriate, @detroit@ should be used instead of
-- some custom error-generating thing.  However, as implied by "where
-- appropriate", custom error messages may be used to throw errors for
-- non-'Response' data.
detroit :: Response BS.ByteString -> a;
detroit k = error $ "Thus spake the homeserver: " ++
            show (getResponseStatusCode k) ++ "; " ++
            show (getResponseBody k) ++ ".";

-- | @justLeft (Left k) == k@.  @justLeft (Right g)@ throws an error.
--
-- @justLeft@ is used because VARIK does not wish to add junk data to
-- @fromLeft@ statements, for VARIK finds that such junk data looks a
-- bit inelegant, unlike this perfectly-wrapped paragraph.
--
-- Compare with @'justRight'@.
justLeft :: Either a b -> a;
justLeft (Left a) = a;
justLeft (Right _) = error "justLeft is applied to a value of type \
                     \'Right'!";

-- | @justRight (Right k) == k@.  @justRight (Left g)@ throws an error.
--
-- @justRight@ is used because VARIK does not wish to add junk data to
-- @fromRight@ statements, for VARIK finds that such junk data looks a
-- bit inelegant, unlike this perfectly-wrapped paragraph.
--
-- Compare with @'justLeft'@.
justRight :: Either a b -> b;
justRight (Right b) = b;
justRight (Left _) = error "justRight is applied to a value of type 'Left'!";

-- | @(a <.> b) k@ is equivalent to @a <$> b k@.
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c;
(<.>) a b c = a <$> b c;

-- | @processError (Left k)@ throws an error whose error message is @k@.
-- | @processError (Right k) == k@.
processError :: Either Stringth a -> a;
processError = either (error . T.unpack) id;

-- | 'StringLike' contains the types which can be converted to and from
-- 'String's.
class StringLike a where
  -- | For all 'StringLike' values @a@, @fromString a@ is a 'String'
  -- which is equivalent to @a@.
  toString :: a -> String
  -- | For all 'StringLike' types @a@, @fromString k :: a@ is an 'a'
  -- value which is equivalent to @k@.
  fromString :: String -> a

instance StringLike BSL.ByteString where
  fromString = BSL.pack . map (toEnum . fromEnum)
  toString = map (toEnum . fromEnum) . BSL.unpack;

instance StringLike BS.ByteString where
  fromString = BS.pack . map (toEnum . fromEnum)
  toString = map (toEnum . fromEnum) . BS.unpack;

instance StringLike T.Text where
  fromString = T.pack
  toString = T.unpack;

-- | @favoriteNoise@ is a pseudorandom 'String' which matches the
-- regular expression @[A-Za-z0-9]{24}@.
--
-- @favoriteNoise@ generates a maximum of (26+26+10)^24, which is
-- approximately equal to 10^43, pseudorandom sequences.  10^43
-- pseudorandom sequences should be sufficient.
favoriteNoise :: IO String;
favoriteNoise = T.unpack <$> stringRandomIO "[A-Za-z0-9]{24}";

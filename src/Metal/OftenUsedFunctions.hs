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
import Text.StringRandom;
import Network.HTTP.Simple;
import qualified Data.Text as T;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BSL;

-- | @(.:)@ is a function composition operator.  The type signature and
-- source code are the best possible documentation.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d;
(.:) g f a b = g $ f a b;

-- | @detroit' k@ displays the status code and body of @k@.
detroit' :: Response BS.ByteString -> String;
detroit' k = "Thus spake the homeserver: " ++
             show (getResponseStatusCode k) ++ "; " ++
             show (getResponseBody k) ++ ".";

-- | @(a <.> b) k@ is equivalent to @a <$> b k@.
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c;
(<.>) a b c = a <$> b c;

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

{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.OftenUsedFunctions
-- Description : Miscellaneous boilerplate functions
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- Metal.OftenUsedFunctions contains some functions which are used by
-- numerous modules of Metal.
module Metal.OftenUsedFunctions where
import Data.Bool;
import Text.StringRandom;
import Network.HTTP.Simple;
import qualified Data.Text as T;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.UTF8 as BS8;
import qualified Data.ByteString.Lazy as BSL;
import qualified Data.ByteString.Lazy.UTF8 as BSL8;

-- | @(.:)@ is a function composition operator.  The type signature and
-- source code are the best possible documentation.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d;
(.:) g f a b = g $ f a b;
infixr 9 .:;

-- | @detroit' k@ displays the status code and body of @k@.
detroit' :: Response BS.ByteString -> String;
detroit' k = "Thus spake the homeserver: " ++
             show (getResponseStatusCode k) ++ "; " ++
             show (getResponseBody k) ++ ".";

-- | @(a <.> b) k@ is equivalent to @a <$> b k@.
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c;
(<.>) a b c = a <$> b c;
infixl 4 <.>;

-- | 'StringLike' contains the types which can be converted to and from
-- 'String's.
--
-- For all 'StringLike's @A@, for all 'String's @t@,
-- @toString (fromString t :: A) == t@.
class StringLike a where
  -- | fromString a@ is a 'String' which is equivalent to @a@.
  toString :: a -> String
  -- | @fromString k :: a@ is an 'a' value which is equivalent to @k@.
  fromString :: String -> a

instance StringLike BSL.ByteString where
  fromString = BSL8.fromString
  toString = BSL8.toString

instance StringLike BS.ByteString where
  fromString = BS8.fromString
  toString = BS8.toString

instance StringLike T.Text where
  fromString = T.pack
  toString = T.unpack;

-- | @favoriteNoise@ is a pseudorandom 'String' which matches the
-- regular expression @[A-Za-z0-9]{24}@.
--
-- @favoriteNoise@ generates a maximum of \((26+26+10)^{24}\), which is
-- approximately equal to \(10^{43}\), pseudorandom sequences.
-- \(10^{43}\) pseudorandom sequences should be sufficient.
favoriteNoise :: IO String;
favoriteNoise = T.unpack <$> stringRandomIO "[A-Za-z0-9]{24}";

-- | If @a@ is an uninterrupted subsequence of @b@, then @bedBathAnd a
-- b@ is the entirety of @b@ which follows the first instance of @a@ in
-- @b@.  If @a@ is not an uninterrupted subsequence of @b@, then
-- @bedBathAnd a b@ is @[]@.
bedBathAnd :: Eq a => [a] -> [a] -> [a];
bedBathAnd _ [] = [];
bedBathAnd x xs = bool (bedBathAnd x xs') xs' $ take (length x) xs == x
  where xs' = drop (length x) xs;

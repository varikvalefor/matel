-- | Metal.OftenUsedFunctions contains some functions which are used by
-- numerous modules of Metal.
module Metal.OftenUsedFunctions where
import Network.HTTP.Simple;
import qualified Data.ByteString as BS;

-- | @detroit k@ breaks such that a description of @k@ is written to the
-- standard error.
detroit :: Response BS.ByteString -> a;
detroit k = error "Thus spake the homeserver: " ++ getResponseCode k ++
            "; " ++ show (getResponseBody k) ++ ".";

-- | @justLeft (Left k) == k@.  @justLeft (Right g)@ throws an error.
--
-- @justLeft@ is used because VARIK does not wish to add junk data to
-- @fromLeft@ statements, for VARIK finds that such junk data looks a
-- bit inelegant, unlike this perfectly-wrapped paragraph.
--
-- Compare with @'justRight'@.
justLeft :: Either a b -> a;
justLeft (Left a) = a;
justLeft (Right _) = error "justLeft is applied to a value of type 'Right'!";

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

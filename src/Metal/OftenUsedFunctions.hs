-- | Metal.OftenUsedFunctions contains some functions which are used by
-- numerous modules of Metal.
module Metal.OftenUsedFunctions where
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

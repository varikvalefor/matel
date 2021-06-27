{- |
 - Module:      $Header$
 - Description: $Header$ contains Matel's basic configuration crap.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -              
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
 -
 - $Header$ contains some "basic" configuration stuff of Matel, e.g.,
 - the authorisation token of this instance of Matel, as well as the
 - username of Matel's user.
 - -}

module Config where
-- | @authToken@ equals the authorisation token which Matel uses to
-- sign in to Matrix.
authToken :: String;
authToken = "";

-- | @username@ equals the username of Matel's user, e.g.,
-- "\@varikvalefor:matrix.org".
username :: String;
username = "";

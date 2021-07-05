{-# LANGUAGE OverloadedStrings #-}

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
 -
 - This method of configuring Matel is used because the alternative,
 - which is reading the configuration information from a dedicated file,
 - e.g., @~/.matelrc@, would imply increasing the argument count of
 - nearly every function which pertains to the Matrix API, thereby
 - reducing the cleanliness of the source code of Matel.
 - -}

module Config where
import Metal.Base;

-- | @authToken@ equals the authorisation token which Matel uses to
-- sign in to Matrix.
authToken :: String;
authToken = "";

-- | @username@ equals the username of Matel's user, e.g.,
-- "\@varikvalefor:matrix.org".
username :: String;
username = "@varikvalefor:matrix.org";

-- | @password@ equals the password of Matel's user, e.g.,
-- "nice try, jack-ass.  What kind of idiot would actually place a
-- password here?".
password :: Stringth;
password = "QSw1sga5z3kBtj98ULNFplyiybgZhuTFyM3G2X4yhx3E70vUjXu49S7C2Cmtt4sj";

{- |
 - Module      :  $Header$
 - Description :  $Header$ defines the TextMess record type.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains the definition of the TextMess record type.
 - -}

module Metal.Messages.Standard where
import Metal.Base;

-- | For all TextMess k, k is an unencrypted or de-encrypted Matrix
-- message.
data TextMess = TextMess {
  -- | For all TextMess k, body k contains the unencrypted body of k.
  body :: MessageText,
  -- | For all TextMess k, sender k is the User-based description of the
  -- sender of k.
  sender :: User,
  -- | For all TextMess k, timestamp k equals the UNIX time-based
  -- timestamp of k, according to the origin server of k.
  timestamp :: UNIXTime
} deriving (Eq, Read, Show);

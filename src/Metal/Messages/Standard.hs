{- |
 - Module      :  $Header$
 - Description :  $Header$ defines the StdMess record type.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains the definition of the StdMess record type.
 - -}

{-# LANGUAGE DeriveAnyClass #-}

module Metal.Messages.Standard where
import Metal.Base;

data MessageType = TextInnit | Image | Attach deriving (Eq, Read, Show);

-- | For all StdMess k, k is an unencrypted or decrypted Matrix
-- message.  k may be a standard text-based message or a message which
-- contains some sort of attachment.
data StdMess = StdMess {
  -- | For all stdMess k, msgType k indicates whether k is text-based or
  -- contains some sort of attachment.
  --
  -- For all StdMess k...
  -- msgType k == TextInnit iff k is text-based.
  -- msgType k == Image iff k contains an embedded image.
  -- msgType k == Attach iff k contains an attachment of some other
  -- type.
  msgType :: MessageType,
  -- | For all StdMess k, body k contains the unencrypted body of k.
  body :: MessageText,
  -- | For all StdMess k, sender k is the User-based description of the
  -- sender of k.
  sender :: User,
  -- | For all StdMess k, timestamp k equals the UNIX time-based
  -- timestamp of k, according to the origin server of k.
  timestamp :: UNIXTime,
  -- | For all StdMess k, fmtBody k is calculated in accordance with the
  -- "formatted_body" field of the source of k.
  fmtBody :: Maybe MessageText,
  -- | For all StdMess k, fmt k equals the content of the "format" field
  -- of the source of k.
  -- fmt k == MtCusHTML iff k is formatted as a HTML message.  Per the
  -- Matrix specification as of 20210605, fmt k may only equal
  -- MatrixCusHTML.
  fmt :: MessageFmt

} deriving (Eq, Mess, Read, Show);

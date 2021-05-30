{- |
 - Module      :  $Header$
 - Description :  $Header$ describes the CryptoMess record type.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains the source code of the CryptoMess record type.
 - -}

module Metal.CryptoMess where
import Metal.Base;
import Data.ByteString as BS;

-- | For all CryptoMess k, k is an encrypted Matrix message.
data CryptoMess = CryptoMess {
  -- | For all CryptoMess k, ciphertext k equals the content of the
  -- "ciphertext" field of k's source.
  ciphertext :: BS.ByteString,
  -- | For all CryptoMess k, algorithm k equals the content of the
  -- "algorithm" field of k's source.
  algorithm :: BS.ByteString,
  -- | For all CryptoMess k, device_id k equals the content of the
  -- "device_id" field of k's source.
  device_id :: BS.ByteString,
  -- | For all CryptoMess k, relates_to k equals the content of the
  -- "event_id" field of the "m.relates_to" dingus of k's source.
  relates_to :: BS.ByteString,
  -- | For all CryptoMess k, sender_key k equals the content of the
  -- "sender_key" field of k's source.
  sender_key :: BS.ByteString,
  -- | For all CryptoMess k, session_id k equals the content of the
  -- "session_id" field of k's source.
  session_id :: BS.ByteString,
  -- | For all CryptoMess k, sender k equals the content of the
  -- "sender" field of k's source.
  sender :: BS.ByteString
} deriving (Eq, Read, Show);

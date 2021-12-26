{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

-- | Module    : Plegg
-- Description : OpenBSD-specific security rubbish
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- This module ensures that when compiled on OpenBSD systems, Matel and
-- MATELCLI support OpenBSD's @pledge(2)@ and @unveil(2)@.
module Plegg where
#ifdef openbsd_HOST_OS
  import Foreign (Ptr, nullPtr);
  import Foreign.C.Error (throwErrnoIfMinus1_);
  import Foreign.C.String (CString, withCString);
  import System.Directory;

  foreign import ccall "unistd.h pledge" pledge :: CString
                                                -> Ptr [CString]
                                                -> IO Int;
  foreign import ccall "unistd.h unveil" unveil :: CString
                                                -> CString
                                                -> IO Int;

  -- | @plegg@ is an extremely high-level interface to @pledge(2)@.  At
  -- this point, just read the source code.
  plegg :: IO ();
  plegg = throwErrnoIfMinus1_ "pledge fails!" $
          withCString "cpath wpath rpath inet dns stdio" $ \premises ->
          pledge premises nullPtr;

  -- | @univac@ is an extremely high-level interface to @unveil(2)@.  At
  -- this point, just read the source code.
  univac :: IO ();
  univac =
    getHomeDirectory >>= \hd ->
    expose (hd ++ "/.config/matel") "rwxc" >>
    -- \| Exposing /dev/random IS necessary; if /dev/random is not
    -- exposed, then the entropy which is used to initiate TLS
    -- connections cannot be generated.
    expose "/dev/random" "rx" >>
    -- \| Exposing this file  is necessary because if this thing is not
    -- exposed, then Matel has no reason to trust that the homeserver is
    -- actually the homeserver, as opposed to being some punk-ass
    -- credential sniffer.
    expose "/etc/ssl/cert.pem" "r"
    where
    expose :: String -> String -> IO ()
    expose path perms =
      throwErrnoIfMinus1_ "unveil hath fallen!" $
      withCString path $ \pathC ->
      withCString perms $ \permsC ->
      unveil pathC permsC;
#else
  -- | @plegg@ does nothing; this module is not compiled on OpenBSD.
  plegg :: IO ();
  plegg = return ();

  -- | @univac@ does nothing; this module is not compiled on OpenBSD.
  univac :: IO ();
  univac = return ();
#endif

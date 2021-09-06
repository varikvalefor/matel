-- Mad props to https://github.com/oherrala for the original version of
-- this module.
--
-- Copyright (c) 2015, Ossi Herrala <oherrala@gmail.com>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
-- notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the copyright holder nor the names of its
-- contributors may be used to endorse or promote products derived from
-- this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
-- OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
-- AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
-- WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Plegg where

#ifdef openbsd_HOST_OS
  import Foreign (Ptr, nullPtr);
  import Foreign.C.Error (throwErrnoIfMinus1_);
  import Foreign.C.String (CString, withCString);
  import System.Directory;

  foreign import ccall "unistd.h pledge" pledge :: CString -> Ptr [CString] -> IO Int;
  foreign import ccall "unistd.h unveil" unveil :: CString -> CString -> IO Int;

  -- | @plegg@ is an extremely high-level interface to @pledge(2)@.  At
  -- this point, just read the source code.
  plegg :: IO ();
  plegg = throwErrnoIfMinus1_ "pledge fails!" $
          withCString "rpath inet dns stdio" $ \premises ->
          pledge premises nullPtr;

  -- | @univac@ is an extremely high-level interface to @unveil(2)@.  At
  -- this point, just read the source code.
  univac :: IO ();
  univac =
    getHomeDirectory >>= \hd ->
    expose (hd ++ "/.config/matel") "rx" >>
    -- \| Exposing /dev/random IS necessary; if /dev/random is not
    -- exposed, then the entropy which is used to initiate TLS
    -- connections cannot be generated.
    expose "/dev/random" "rx" >>
    -- \| Exposing this file  is necessary because if this thing is not
    -- exposed, then Matel has no reason to trust that the homeserver is
    -- actually the homeserver, as opposed to being some lame-ass
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
  plegg :: IO ();
  plegg = return ();
#endif

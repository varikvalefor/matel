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
  import Data.List (intersperse, nub);
  import Data.Char (toLower);
  import Foreign (Ptr, nullPtr);
  import Foreign.C.Error (throwErrnoIfMinus1_);
  import Foreign.C.String (CString, withCString);
  import System.FilePath (FilePath);

  foreign import ccall "unistd.h pledge" c_pledge :: CString -> Ptr [CString] -> IO Int;

  -- | Promises is an alias of [Promise].
  type Promises = [Promise];

  -- | For all Promise k, k is a promise of pledge(2).  Documentation of
  -- such promises is available in manual page pledge(2).
  data Promise = Rpath    | Wpath     | Cpath  | Stdio | Tmppath | Dns     | Inet   | Flock
               | Unix     | Id        | Ioctl  | Getpw | Proc    | Settime | Fattr  | Protexec
               | Tty      | Sendfd    | Recvfd | Exec  | Route   | Mcast   | Vminfo | Ps
               | Coredump | Disklabel | Pf     | None
               deriving (Eq, Show);

  -- | For all Promises k, for all [FilePath] g, pledge k g is roughly equivalent
  -- to the C pledge(prmises k, g).  However, the whitelisting of filepaths is
  -- currently unsupported.
  pledge :: Promises -> [FilePath] -> IO ();
  -- special case for completely empty pledge. Useful? Maybe not.
  pledge [] _ = throwErrnoIfMinus1_ "pledge" $ c_pledge nullPtr nullPtr;
  -- Generic case, but we don't support giving whilelist of paths yet
  pledge proms [] =
    withCString (promises proms) $ \c_proms ->
        let c_paths = nullPtr in
              throwErrnoIfMinus1_ "pledge" $ c_pledge c_proms c_paths;
  pledge _ _ = error "Pledge does not support [FilePath].";

  -- | For all Promise k, promise k equals a lowercase String representation
  -- of k.
  promise :: Promise -> String;
  promise = map toLower . show;

  -- | For all Promises k, promises k equals a String representation of k which
  -- is compatible with C's pledge.
  promises :: Promises -> String;
  promises [] = "";
  promises proms 
    | None `elem` proms = ""
    | otherwise = unwords $ map promise $ nub proms;

  plegg :: IO ();
  plegg = pledge [Rpath, Dns, Inet, Stdio] [];
#else
  plegg :: IO ();
  plegg = return ();
#endif

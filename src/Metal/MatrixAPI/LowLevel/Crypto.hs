-- | Module    : Metal.MatrixAPI.LowLevel.Crypto
-- Description : Cryptographic crap for Matrix
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.MatrixAPI.LowLevel.Crypto contains cryptographic crap.
module Metal.MatrixAPI.LowLevel.Crypto where
import Data.Maybe;
import Metal.Base;
import Crypto.Error;
import qualified Data.Text as T;
import Metal.OftenUsedFunctions;
import qualified Data.ByteString as BS;
import qualified Crypto.PubKey.Curve25519 as X25519;

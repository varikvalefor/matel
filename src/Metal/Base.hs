-- | Module    : Metal.Base
-- Description : Metal's basic datatypes and whatnot
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.Base contains the core data types of Metal.
module Metal.Base (
  -- * High-Level Message-Data-Centric Stuff
  --
  -- $highLevelMessageDataCrap
  Identifier,
  HumanReadableName,
  MessageText,
  UNIXTime,
  ByteData,
  CipherByteData,
  -- * Key Containment Types
  --
  -- $keyContainers
  Keyring,
  Key,
  PublicKey,
  PrivateKey,
  SharedSecret,
  -- * Consistency and Readability
  --
  -- $convenience
  Stringth,
  ErrorCode,
  -- * High-Level Datatypes
  AlGoreRhythm (..),
  MessageFmt (..)
) where
import qualified Data.Text as T;

-- $highLevelMessageDataCrap
--
-- This section of the module contains some type aliases which
-- essentially just improve the readability of Metal's source code.

-- | For all 'Identifier' @k@, @k@ is a "non-human-readable" name, e.g.,
-- "\@johnnykissass:matrix.varikose.god", as opposed to "Johnny
-- Kissass".
--
-- Note that the set of all MXIDs is a proper subset of the set of all
-- 'Identifier's.
type Identifier = String;
-- | For all 'HumanReadableName' @x@, @x@ is a "human-readable" name,
-- e.g., "Asshat", as opposed to "\@asshat:matrix.varikose.god".
type HumanReadableName = Stringth;
-- | Unencrypted\/de-encrypted text-based message data is of type
-- 'MessageText'.  This comment almost fit on one line.  #SAD!
type MessageText = Stringth;
-- | For all 'UNIXTime' @k@, @k@ is a seconds-since-the-UNIX-epoch-based
-- timestamp.  The time zone is not standardised.
--
-- Unlike the clocks of some crappy-ass operating systems, 'UNIXTime' is
-- not subject to the Year 2038 problem.  Ave 'Integer'!
type UNIXTime = Integer;
-- | For all 'ByteData' @k@, @k@ is some data which is represented as a
-- string of bytes.  @k@ most likely contains text but may contain some
-- other type of data, e.g., a PNG file.
type ByteData = Stringth;
-- | For all 'CipherByteData' @k@, @k@ is an encrypted sequence of
-- bytes.  The decrypted @k@ is of type 'ByteData'.
type CipherByteData = Stringth;

-- $keyContainers
--
-- This portion of the module contains datatypes which hold encryption
-- keys.

-- | For all 'PublicKey' @g@, @g@ is a public key.
type PublicKey = Stringth;
-- | For all 'PrivateKey' @g@, @g@ is a private key.
type PrivateKey = Stringth;
-- | For all 'SharedSecret' @k@, @k@ is a shared secret key.
type SharedSecret = Stringth;

-- $convenience
--
-- This portion of the module contains some type aliases which are added
-- for the sake of readability and consistency.

-- | 'Stringth' is equivalent to 'T.Text' and added only for the sake of
-- convenience.
type Stringth = T.Text;
-- | 'ErrorCode' is used to contain descriptions of functions' errors,
-- e.g., "the message cannot be posted; the homeserver is off-line."
type ErrorCode = T.Text;

-- $highLevelDataTypes
--
-- This section of the module contains some high-level "proper"
-- datatypes.

-- | 'AlGoreRhythm's represent cryptographic systems, e.g., Olm and
-- Megolm.
data AlGoreRhythm = Olm
                  | Megolm
                  deriving (Eq);

-- | For all 'MessageFmt' @x@, @x@ is a message type, as defined by the
-- Matrix client-server specification.
data MessageFmt = MatrixCusHTML
                  -- ^ This value corresponds to the client-server
                  -- specification's "@org.matrix.custom.html@" format.
  deriving (Eq, Read, Show);

-- | 'Keyring's are just collections of 'Key's.
type Keyring = [Key];

-- | For all 'Key's @a@, @a@ technically represents a key/pair/, as
-- opposed to a single /key/.  However, the verbosity of "KeyPair" is
-- greater than the verbosity of "Key", and the distinction is not /too/
-- important, anyway.
--
-- = Fistful of Monads
--
-- Placing both elements of the 2-tuple into the 'Maybe' monad may at
-- first glance appear to be redundant, as for all keys @a@, the public
-- key of @a@ should be known.  However, for all 'Key's @a@, that @a@
-- contains only a public key is possible.
--
-- == "But Why is the Public Key 'Maybe'-Monadic?"
--
-- The public key is 'Maybe'-monadic because some 'Key' values /can/
-- reasonably lack a public key.
--
-- === "But WHY?!?!"
--
-- Matel, which is the primary user of Metal and, by extension,
-- "Metal.Keys", saves the keys which Matel's user has created... but
-- only saves the private keys; for all Matrix private keys @a@, the
-- public key which encrypts stuff for @a@ can be calculated if @a@ is
-- known.  This approach is relatively storage-cheap and
-- hacker-friendly.
type Key = (Maybe PublicKey, Maybe PrivateKey);

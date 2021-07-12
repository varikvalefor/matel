{- |
 - Module:      $Header$
 - Description: $Header$ contains Metal's core data types.
 - Copyright:   (c) Varik Valefor
 - License:     BSD-3-Clause
 -
 - Maintainer:  varikvalefor@aol.com
 - Stability:   unstable
 - Portability: portable
 -
 - $Header$ contains the core data types of Metal.
 - -}
 
module Metal.Base where
import qualified Data.Text as T;

-- DATATYPES -----------------------------------------------------------

-- | For all 'Identifier' @k@, @k@ is a "non-human-readable" name, e.g.,
-- "\@johnnykissass:matrix.varikose.god", as opposed to "Johnny Kissass".
type Identifier = String;
-- | For all 'HumanReadableName' @x@, @x@ is a "human-readable" name,
-- e.g., "Asshat", as opposed to "\@asshat:matrix.varikose.god".
type HumanReadableName = Stringth;
-- | Unencrypted/de-encrypted text-based message data is of type
-- 'MessageText'.  This comment almost fit on one line.  #SAD!
type MessageText = Stringth;
-- | For all 'UNIXTime' @k@, @k@ is a seconds-since-the-UNIX-epoch-based
-- timestamp.  The time zone is not standardised.
type UNIXTime = Integer;
-- | For all 'ByteData' @k@, @k@ is some data which is represented as a
-- string of bytes.  @k@ most likely contains text but may contain some
-- other type of data, e.g., a PNG file.
type ByteData = Stringth;
-- | For all 'CipherByteData' @k@, @k@ is an encrypted sequence of
-- bytes.  The decrypted @k@ is of type 'ByteData'.
type CipherByteData = Stringth;
-- | For all 'PublicKey' @g@, @g@ is a public key.
type PublicKey = Stringth;
-- | For all 'PrivateKey' @g@, @g@ is a private key.
type PrivateKey = Stringth;
-- | 'Stringth' is equivalent to 'T.Text' and added only for the sake of
-- convenience.
type Stringth = T.Text;
-- | 'ErrorCode' is used to contain descriptions of functions' errors,
-- e.g., "the message cannot be posted; the homeserver is off-line."
type ErrorCode = [Char];

-- | For all @x@, @x@ is of type 'MessageFmt' iff @x@ is a message type,
-- as defined by the Matrix client-server specification.
data MessageFmt = MatrixCusHTML deriving (Eq, Read, Show);

-- | The 'Mess' class contains all messages.
class Mess a;

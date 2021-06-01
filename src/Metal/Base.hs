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
import Brick;
import qualified Data.ByteString as BS;

-- DATATYPES -----------------------------------------------------------

-- | For all Identifier k, k is a "non-human-readable" name, e.g.,
-- "@johnnykissass:matrix.varikose.god", as opposed to "Johnny Kissass".
type Identifier = Stringth;
-- | For all HumanReadableName x, x is a "human-readable" name, e.g.,
-- "Asshat", as opposed to "@asshat:matrix.varikose.god".
type HumanReadableName = Stringth;
-- | Unencrypted/de-encrypted text-based message data is of type
-- MessageText.  This comment almost fit on one line.  #SAD!
type MessageText = Stringth;
-- | Screen is equivalent to Brick's Widget ().
-- For all Screen x, x is TUI data which should be immediately drawn.
type Screen = Widget ();
-- | Winda is equivalent to Brick's Widget ().
-- For all Winda k, k is a Widget which should NOT be immediately drawn
-- to the terminal; k requires additional processing.
type Winda = Widget ();
-- | For all users k, k is represented as a 2-tuple User l, with fst l
-- representing the @username:homeserver.whatevs-based identifier of k
-- and snd l representing the human-readable name of k, e.g., "Asshat".
type User = (Identifier, HumanReadableName);
-- | For all UNIXTime k, k is a seconds-since-the-UNIX-epoch-based
-- timestamp.  The time zone is not standardised.
type UNIXTime = Integer;
-- | For all ByteData k, k is some data which is represented as a string
-- of bytes.  k most likely contains text but may contain some other
-- type of data, e.g., a PNG file.
type ByteData = Stringth;
-- | For all CipherByteData k, k is an encrypted sequence of bytes.
-- The decrypted k is of type ByteData.
type CipherByteData = Stringth;
-- | For all PublicKey g, g is a public key.
type PublicKey = Stringth;
-- | For all PrivateKey g, g is a private key.
type PrivateKey = Stringth;
-- | Stringth is equivalent to BS.ByteString and added only for the sake
-- of convenience.
type Stringth = BS.ByteString;
-- | ErrorCode is used to contain descriptions of functions' errors,
-- e.g., "the message cannot be posted; the homeserver is off-line."
type ErrorCode = [Char];

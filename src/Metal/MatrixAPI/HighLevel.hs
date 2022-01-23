 {-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.HighLevel
-- Description : Metal's high-level Matrix API stuff
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- Metal.MatrixAPI.HighLevel contains functions which use the Matrix
-- API by chaining together relatively low-level functions for the
-- Matrix API.
--
-- This module differs from Metal.MatrixAPI.LowLevel because the
-- functions within this module transparently support encryption and do
-- not explicitly use HTTP queries, whereas the functions of
-- Metal.MatrixAPI.LowLevel generally explicitly use HTTP queries and
-- support only explicit encryption.
--
-- Additionally, several functions which this module provides just break
-- via @'error'@ if anything goes wrong, as opposed to returning an
-- error code or 'Nothing'.  However, VARIK is willing to modify this
-- module's functions such that these functions do not simply break
-- if such a change would benefit any users of this module.
module Metal.MatrixAPI.HighLevel (
  -- * Stuff-Fetching Functions
  --
  -- $stuffFetch
  recentMessagesFrom,
  earlyMessagesFrom,
  memberRooms,
  memberSpaces,
  memberComms,
  -- * Stuff which is Imported from Metal.MatrixAPI.LowLevel
  --
  -- $stuffImport
  sendEvent,
  ban,
  unban,
  kick,
  leave,
  -- Metal.MatrixAPI.LowLevel.join and Control.Monad.join collide.
  Metal.MatrixAPI.LowLevel.join,
  sync,
  loginPass,
  upload,
  createRoom,
  -- * Stuff-Sending Functions
  --
  -- $ stuffSend
  send,
  markRead,
) where
import Data.Bool;
import Data.List;
import Data.Maybe;
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.Space;
import Control.Monad;
import Metal.Community;
import Metal.Encrypted;
import Data.Either as EE;
import Metal.EventCommonFields;
import Metal.MatrixAPI.LowLevel;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import Metal.Messages.Standard as MS;
import qualified Metal.Default as Def;
import Metal.MatrixAPI.LowLevel.FetchEvents;

-- $stuffImport
--
-- This section of this module contains some functions which are
-- imported from Metal.MatrixAPI.LowLevel.  These functions are
-- exported exactly as these functions appear in
-- Metal.MatrixAPI.LowLevel because writing wrappers for these
-- functions would be a fairly pointless process; these functions
-- are already reasonably high-level.

-- Ain't nothin' here, foo'.  Can't you read?

-- $stuffFetch
--
-- This section of the module contains some functions which gather
-- information, e.g., lists of 'Rooms' of which Matel's user is a
-- member.

-- | @recentMessagesFrom@ fetches the messages which are most recently
-- sent to a Matrix room.
--
-- = Output
--
-- If the messages are fetched correctly, then these messages are
-- returned as a 'Right' ['StdMess'].
-- If the messages are not fetched correctly, then a 'Left' 'ErrorCode'
-- is returned.
--
-- = Lack of Support for Encrypted Messages
--
-- @recentMessagesFrom@ currently does not support the fetching of
-- encrypted messages.
recentMessagesFrom :: Integer
                   -- ^ This argument is the number of messages which
                   -- are fetched.
                   -> Room
                   -- ^ This argument specifies the Matrix room whose
                   -- messages are fetched.
                   -- fetched
                   -> Auth
                   -- ^ This argument is the same old authorisation
                   -- bullshit.
                   -> IO (Either ErrorCode [StdMess]);
recentMessagesFrom = flip fetchMessages 'b';

-- | @fetchMessages@ fetches encrypted and unencrypted messages.
--
-- = Output
--
-- If the messages are fetched correctly, then the output is a
-- 'Right'-valued list of the decrypted and unencrypted messages which
-- are fetched.  Otherwise, the output is a 'Left' 'ErrorCode' which
-- describes the problem which prevents the fetching of messages.
fetchMessages :: Integer
              -- ^ This bit is the number of messages which are fetched.
              -> Char
              -- ^ This bit represents the direction in which messages
              -- are fetched.
              --
              -- If this argument equals @\'b\'@, then the most recent
              -- messages are fetched.
              --
              -- If this argument equals @\'f\'@, then the earliest
              -- messages are fetched.
              -> Room
              -- ^ This argument represents the room whose messages
              -- are fetched.
              --
              -- The @roomId@ value must be defined.
              -> Auth
              -- ^ This bit is the same old authorisation junk.
              -> IO (Either ErrorCode [StdMess]);
fetchMessages n dir r a = liftM2 combin8 grabUnencrypted grabDecrypted
  where
  grabUnencrypted :: IO (Either ErrorCode [StdMess])
  grabUnencrypted = fetchEvents n dir Def.stdMess r a
  --
  grabDecrypted :: IO (Either ErrorCode [StdMess])
  grabDecrypted = fmap (>>= decryptAll) grabEncrypted
  --
  grabEncrypted :: IO (Either ErrorCode [Encrypted])
  grabEncrypted = fetchEvents n dir Def.encrypted r a
  -- \| @decryptAll j@ 'Right'ly returns a null list because having
  -- @fetchMessages@ break at this point can be a bit annoying.
  --
  -- TODO: IMPLEMENT PROPER DECRYPTION.
  decryptAll :: [Encrypted] -> Either ErrorCode [StdMess]
  decryptAll _ = Right []
  --
  combin8 :: Either ErrorCode [StdMess]
          -> Either ErrorCode [StdMess]
          -> Either ErrorCode [StdMess]
  combin8 b = fmap (sortOn timestamp) . liftM2 (++) b
  --
  timestamp :: StdMess -> UNIXTime
  timestamp = origin_server_ts . MS.boilerplate;

-- | @earlyMessagesFrom@ fetches the messages which are first sent to a
-- Matrix room.
--
-- = Output
--
-- If the messages are fetched correctly, then these messages are
-- returned as a 'Right' ['StdMess'].
-- If the messages are not fetched correctly, then a 'Left' 'ErrorCode'
-- is returned.
--
-- = Lack of Support for Encrypted Messages
--
-- @earlyMessagesFrom@ currently does not support the fetching of
-- encrypted messages.
earlyMessagesFrom :: Integer
                  -- ^ This argument is the number of messages which
                  -- should be fetched.
                  -> Room
                  -- ^ This argument represents the Matrix room from
                  -- which messages are nabbed.
                  -> Auth
                  -- ^ This bit is the same old authorisation bullshit.
                  -> IO (Either ErrorCode [StdMess]);
earlyMessagesFrom = flip fetchMessages 'f';

-- | @memberRooms@ nabs a list of rooms of which Matel's user is a
-- member.
--
-- = Output
--
-- The output is an IO-monadic list of the 'Rooms' of which Matel's user
-- is a member.
--
-- = Exception Handling
--
-- @memberRooms@ may throw an error and burst into flames.  Feel free to
-- request the removal of this error functionality if this error
-- functionality is found to be inconvenient.
memberRooms :: Auth
            -- ^ This argument is the authorisation information of
            -- the user.
            -> IO [Room];
memberRooms bugspray = joinedRooms bugspray >>= maybeShowRms
  where
  listRoomsMentioned :: Either ErrorCode [Room]
                     -> IO (Either ErrorCode [Room])
  listRoomsMentioned  = either (pure . Left) actuallyNab
  --
  actuallyNab :: [Room] -> IO (Either ErrorCode [Room])
  actuallyNab = dl <.> mapM (flip getRoomInformation bugspray)
  -- \| "dl" is an abbreviation of "de-list".
  dl :: [Either ErrorCode Room] -> Either ErrorCode [Room]
  dl j = bool (Left $ head $ lefts j) (Right $ rights j) $ any isLeft j
  --
  maybeShowRms :: Either ErrorCode [Room] -> IO [Room]
  maybeShowRms = bifsram <.> listRoomsMentioned
  -- \| "Break if some rooms are missing."
  bifsram :: Either ErrorCode [Room] -> [Room]
  bifsram = either (error . T.unpack) id

-- | @memberSpaces@ returns a list of the 'Space's of which a user is a
-- member.
--
-- = Exception Handling
--
-- @memberSpaces@ may throw an error and burst into flames.  Feel free
-- to request the removal of this error functionality if this error
-- functionality is found to be inconvenient.
memberSpaces :: Auth
             -- ^ This bit is the authorisation information of the
             -- Matrix user whose joined spaces should be fetched.
             --
             -- This user is PROBABLY the user of Matel.
             -> IO [Space];
memberSpaces = idOrError <.> joinedSpaces;

-- | @memberComms@ returns a list of the 'Community's --
-- EEUUUAAaaARGH -- of which a user is a member.
--
-- = Exception Handling
--
-- @memberComms@ may throw an error and burst into flames.  Feel free to
-- request the removal of this error functionality if this error
-- functionality is found to be inconvenient.
memberComms :: Auth
            -- ^ This bit is the authorisation information of the user
            -- whose 'Community's -- again, EEUUUAAaaARGH -- are listed.
            -> IO [Community];
memberComms = idOrError <.> joinedComms;

-- | @idOrError (Right k) == k@.  @idOrError (Left k)@ throws an 'error'
-- whose message is @k@.
idOrError :: Either ErrorCode a -> a;
idOrError = either (error . T.unpack) id;

-- | @markRead@ marks messages as having been read.
--
-- @markRead@ is currently nonfunctional.
--
-- = Output
--
-- @markRead@ returns 'Nothing' if no problem is encountered.
--
-- However, if a problem is encountered, then @markRead@ 'Just' returns
-- a description of this problem.
markRead :: StdMess
         -- ^ This argument represents the message which should be
         -- marked as having been "read". 
         --
         -- The @messageId@ field of this argument MUST be defined and
         -- valid; if @messageId@ is undefined or invalid, then
         -- @markRead@ may be reduced to a small pile of leaf-rolling
         -- weevils.  But such behaviour is not guaranteed.
         -> Auth
         -- This bit is the authorisation stuff which has already been
         -- documentated 87956 times.
         -> IO (Maybe ErrorCode);
markRead _ _ = pure $ Just "markRead is unimplemented.";

-- $stuffSend
--
-- This section of the module contains the functions of this module
-- which are responsible for the sending of stuff, e.g., messages.

-- | @send@ sends messages to encrypted and unencrypted Matrix rooms.
--
-- = Encryption
--
-- If the room to which the message is sent is encrypted, then an
-- encrypted version of this message is sent to the room.  Otherwise, a
-- mostly unaltered version of the message is sent.
--
-- = Output
--
-- If an error is encountered, then 'Just' a description of this error
-- is returned.  Otherwise, 'Nothing' is returned.
send :: StdMess
     -- ^ This argument is a representation of the message which is to
     -- be sent.
     -> Room
     -- ^ This argument specifies the Matrix room to which the message
     -- should be sent.  Only the @roomId@ field must be non-default.
     -> Auth
     -- ^ The authorisation garbage which is used to send the message...
     -- blah, blah, blah, blah, blah... boilerplate crap...
     -> IO (Maybe ErrorCode);
send event italy a = maybeEncrypt >>= either blowUp jstdt
  where
  -- \| "Just send the damned thing!"
  jstdt = either (\e -> sendEvent e italy a) (\e -> sendEvent e italy a)
  maybeEncrypt :: IO (Either ErrorCode (Either StdMess Encrypted))
  maybeEncrypt = getRoomInformation italy a >>= either (pure . Left) process
  blowUp = return . Just
  encryptFor foo = either Left (Right . Right) <$> roomEncrypt event foo
  process dullards = if isNothing (publicKey dullards)
                       -- \| These dullards can AT LEAST use
                       -- encryption... allegedly.
                       then encryptFor dullards
                       -- \| man yall dullards cant even use encryption
                       -- what a scam
                       -- dang
                       else pure $ Right $ Left event;

-- | @roomEncrypt@ encrypts messages for Matrix rooms.
roomEncrypt :: StdMess
            -- ^ This bit is the message which should be encrypted.
            -> Room
            -- ^ This argument specifies the room for which the message
            -- should be encrypted.
            -> IO (Either ErrorCode Encrypted);
roomEncrypt _ _ = pure $ Left "roomEncrypt is unimplemented.";

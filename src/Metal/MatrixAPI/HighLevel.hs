 {-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.HighLevel
-- Description : Metal's high-level Matrix API stuff
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : unstable
-- Portability : portable
--
-- "Metal.MatrixAPI.HighLevel" contains functions which use the Matrix
-- API by chaining together relatively low-level functions for the
-- Matrix API.
--
-- This module differs "from Metal.MatrixAPI.LowLevel" because the
-- functions within "Metal.MatrixAPI.HighLevel" transparently support
-- encryption and do not explicitly use HTTP queries, whereas the
-- functions of "Metal.MatrixAPI.LowLevel" generally explicitly use HTTP
-- queries and support only explicit encryption.
module Metal.MatrixAPI.HighLevel (
  -- * Stuff-Fetching Functions
  --
  -- $stuffFetch
  fetchMessages,
  recentMessagesFrom,
  earlyMessagesFrom,
  memberRooms,
  memberSpaces,
  memberComms,
  -- * Stuff which is Imported from "Metal.MatrixAPI.LowLevel"
  --
  -- $stuffImport
  sendEvent,
  ban,
  unban,
  kick,
  leave,
  -- \| Metal.MatrixAPI.LowLevel.join and Control.Monad.join collide.
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
import Metal.Messages.Encrypted;
import Data.Either as EE;
import Metal.EventCommonFields;
import Metal.MatrixAPI.LowLevel;
import Metal.OftenUsedFunctions;
import Metal.Messages.Standard as MS;
import Metal.MatrixAPI.LowLevel.FetchEvents;

-- $stuffImport
--
-- This section of this module contains some functions which are
-- imported from "Metal.MatrixAPI.LowLevel".  These functions are
-- exported exactly as these functions appear in
-- "Metal.MatrixAPI.LowLevel" because writing wrappers for these
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

-- | @fetchMessages@ is used to fetch messages from Matrix rooms.
--
-- = Specification
--
-- == 'Right' Output ==
--
-- If the fetching and whatnot is successful, then @fetchMessages n d r
-- a@ uses the authorisation information which is contained within @a@
-- to fetch and return a @d@-directional list of messages which are sent
-- to the room which is represented by @r@.  The length of the list is
-- less than or equal to @n@.
--
-- If the fetching and whatnot is not successful, then @fetchMessages n
-- d r a@ returns a 'Left' 'ErrorCode' which explains the lack of
-- success.
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
              -- The 'roomId' value must be defined.
              -> Auth
              -- ^ This bit is the same old authorisation junk.
              -> IO (Either ErrorCode [StdMess]);
fetchMessages n dir r a = liftM2 combin8 grabUnencrypted grabDecrypted
  where
  grabUnencrypted :: IO (Either ErrorCode [StdMess])
  grabUnencrypted = fetchEvents n dir r a
  --
  grabDecrypted :: IO (Either ErrorCode [StdMess])
  grabDecrypted = fmap (>>= dl . map (`decrypt` a)) grabEncrypted
  --
  grabEncrypted :: IO (Either ErrorCode [Encrypted])
  grabEncrypted = fetchEvents n dir r a
  --
  combin8 :: Either ErrorCode [StdMess]
          -> Either ErrorCode [StdMess]
          -> Either ErrorCode [StdMess]
  combin8 = liftM2 (sortOn timestamp .: (++))
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
-- = Internal Stuff
--
-- @earlyMessagesFrom@ is really just a wrapper for @fetchMessages@.
-- The reader of /this/ piece of documentation should probably /also/
-- read the documentation of 'fetchMessages'.
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
-- The output is an 'Either' an IO-monadic list of the 'Room's of which
-- Matel's user is a member or a reason for the output's not being such
-- a list.
memberRooms :: Auth
            -- ^ This argument is the authorisation information of
            -- the user.
            -> IO (Either ErrorCode [Room]);
memberRooms bugspray = joinedRooms bugspray >>= nabIfSuccessful
  where
  nabIfSuccessful = either (pure . Left) actuallyNab
  --
  actuallyNab :: [Room] -> IO (Either ErrorCode [Room])
  actuallyNab = dl <.> mapM (flip getRoomInformation bugspray);

-- | If @k@ contains a 'Left' value, then @dl k@ is the 'Left'-contained
-- first 'Left' value of @k@.  @dl k@ is otherwise a 'Right' list of the
-- values which are contained within the 'Either's of @k@.
--
-- "@dl@" is an abbreviation of "de-list".
dl :: [Either a b] -> Either a [b];
dl j = bool (Right $ rights j) (Left $ head $ lefts j) $ any isLeft j;

-- | @memberSpaces@ returns a list of the 'Space's of which a user is a
-- member.
--
-- @memberSpaces@ is really just a synonym of 'joinedSpaces'.
--
-- = Output
--
-- If everything goes according to plan, then the list is 'Right'ly
-- returned.  If something fails, then a 'Left' 'ErrorCode' which
-- describes this failure is returned.
memberSpaces :: Auth
             -- ^ This bit is the authorisation information of the
             -- Matrix user whose joined spaces should be fetched.
             --
             -- This user is PROBABLY the user of Matel.
             -> IO (Either ErrorCode [Space]);
memberSpaces = joinedSpaces;

-- | If everything works, then @memberComms t@ 'Right'ly returns a list
-- of the 'Community's -- EEUUUAAaaARGH -- which are joined by the user
-- whose correct authorisation information is contained by @t@.  If
-- @memberComms@ breaks, then @memberComms t@ returns a 'Left'
-- 'ErrorCode' which describes the breakage.
memberComms :: Auth
            -- ^ This bit is the authorisation information of the user
            -- whose 'Community's -- again, EEUUUAAaaARGH -- are listed.
            -> IO (Either ErrorCode [Community]);
memberComms = joinedComms;

-- | @markRead@ is used to mark messages as having been read.
--
-- @markRead@ is currently nonfunctional.
--
-- = Specification
--
-- If @markRead s a@ successfully uses @a@ to mark the message whose
-- event ID is @eventId (boilerplate s)@ as having been read, then
-- @markRead s a@ returns 'Nothing'.  If some failure is encountered,
-- then @markRead s a@ returns a 'Just' and deserved description of
-- this failure.
markRead :: StdMess
         -- ^ This argument represents the message which should be
         -- marked as having been "read".
         --
         -- The 'eventId' field of the 'boilerplate' portion of this
         -- argument MUST be defined and valid; if the 'eventId' is
         -- undefined or invalid, then @markRead@ may be reduced to a
         -- small pile of leaf-rolling weevils.  But such behaviour is
         -- not guaranteed.
         -> Auth
         -- This bit is the authorisation stuff which has already been
         -- documentated 87956 times.
         -> IO (Maybe ErrorCode);
markRead _ _ = pure $ Just "markRead is unimplemented.";

-- $stuffSend
--
-- This section of the module contains the functions of this module
-- which are responsible for the sending of stuff, e.g., messages.

-- | @send@ is used to messages to encrypted and unencrypted Matrix
-- rooms.
--
-- = Specification
--
-- == Encryption
--
-- If the room to which the message is sent is encrypted, then an
-- encrypted version of this message is sent to the room.  Otherwise, a
-- mostly unaltered version of the message is sent.
--
-- == Output
--
-- If an error is encountered, then 'Just' a description of this error
-- is returned.  Otherwise, 'Nothing' is returned.
send :: StdMess
     -- ^ This argument is a representation of the message which is to
     -- be sent.
     -> Room
     -- ^ This argument specifies the Matrix room to which the message
     -- should be sent.  Only the 'roomId' field must be non-default.
     -> Auth
     -- ^ The authorisation garbage which is used to send the message...
     -- blah, blah, blah, blah, blah... boilerplate crap...
     -> IO (Maybe ErrorCode);
send event italy a = maybeCrp >>= either (pure . pure) jstdt
  where
  -- \| "Just send the damned thing!"
  jstdt = either (\e -> sendEvent e italy a) (\e -> sendEvent e italy a)
  -- \| Including the type signature of @maybeCrp@ facilitates
  -- understanding the purpose of @maybeCrp@.  The reader should
  -- probably not remove this type signature.
  maybeCrp :: IO (Either ErrorCode (Either StdMess Encrypted))
  maybeCrp = getRoomInformation italy a >>= either (pure . Left) process
  encryptFor foo = either Left (Right . Right) <$> roomEncrypt event foo
  process dullards = if isNothing (publicKey dullards)
                       -- \| man yall dullards cant even use encryption
                       -- what a scam
                       -- dang
                       then pure $ Right $ Left event
                       -- \| These dullards can AT LEAST use
                       -- encryption... allegedly.
                       else encryptFor dullards;

-- | @roomEncrypt@ is used to encrypt messages for Matrix rooms.
--
-- = Specification
--
-- If something 'splodes, then @roomEncrypt a b@ returns a 'Left'
-- 'ErrorCode' which describes this 'splosion.  If all is 'Right',
-- then @roomEncrypt a b@ returns an 'Encrypted' version of @a@ which
-- can be decrypted by the members of the room which is represented by
-- @b@.
roomEncrypt :: StdMess
            -- ^ This bit is the message which should be encrypted.
            -> Room
            -- ^ This argument specifies the room for which the message
            -- should be encrypted.
            -> IO (Either ErrorCode Encrypted);
roomEncrypt _ _ = pure $ Left "roomEncrypt is unimplemented.";

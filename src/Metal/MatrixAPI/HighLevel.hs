 {-# LANGUAGE OverloadedStrings #-}

-- | Module    : Metal.HighLevel
-- Description : Metal's high-level Matrix API stuff
-- Copyright   : (c) Varik Valefor, 2021
-- License     : BSD-3-Clause
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
import Data.Maybe;
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.Space;
import Control.Monad;
import Metal.Community;
import Metal.Encrypted;
import Metal.Messages.Standard;
import Metal.MatrixAPI.LowLevel;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Either as EE;
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
-- = Arguments
--
-- The first argument is the number of messages which should be output.
--
-- The second argument represents the Matrix room from which messages
-- are grabbed.
--
-- The third argument is the same old authorisation bullshit.
--
-- = Lack of Support for Encrypted Messages
--
-- @recentMessagesFrom@ currently does not support the fetching of
-- encrypted messages.
recentMessagesFrom :: Integer
                   -- ^ The number of messages which should be fetched
                   -> Room
                   -- ^ The room from which the messages should be
                   -- fetched
                   -> Auth
                   -- ^ Authorisation crap
                   -> IO [StdMess];
recentMessagesFrom n = fetchEvents n 'b' Def.stdMess;

-- | @earlyMessagesFrom@ fetches the messages which are first sent to a
-- Matrix room.
--
-- = Arguments
--
-- The first argument is the number of messages which should be output.
--
-- The second argument represents the Matrix room from which messages
-- are grabbed.
--
-- The third argument is the same old authorisation bullshit.
--
-- = Lack of Support for Encrypted Messages
--
-- @earlyMessagesFrom@ currently does not support the fetching of
-- encrypted messages.
earlyMessagesFrom :: Integer
                  -- ^ The number of messages which should be fetched
                  -> Room
                  -- ^ The room from which messages should be fetched
                  -> Auth
                  -- ^ The authorisation details with which messages are
                  -- fetched
                  -> IO [StdMess];
earlyMessagesFrom n = fetchEvents n 'b' Def.stdMess;

-- | @memberRooms@ nabs a list of rooms of which Matel's user is a
-- member.
--
-- = Arguments
--
-- The first -- and only -- argument is the authorisation information of
-- Matel's user.
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
            -- ^ The information which is used to authenticate Matel's
            -- user
            -> IO [Room];
memberRooms bugspray = joinedRooms bugspray >>= maybeShowRms
  where
  listRoomsMentioned :: Either Stringth [Room]
                     -> IO [Either Stringth Room]
  listRoomsMentioned = either convS (mapM (flip getRoomInformation bugspray))
  --
  convS :: Stringth -> IO [Either Stringth Room]
  convS = return . return . Left
  --
  maybeShowRms :: Either Stringth [Room] -> IO [Room]
  maybeShowRms = listRoomsMentioned >=> bifsram
  -- \| "Break if some rooms are missing."
  bifsram :: [Either Stringth Room] -> IO [Room]
  bifsram t = bool err (pure $ map justRight t) $ any EE.isLeft t
    where err = error $ T.unpack $ justLeft $ head $ filter EE.isLeft t;
    -- \^ @filter EE.isLeft@ is used to ensure that the fetched 'Left'
    -- value actually exists; some values may be 'Right'-valued.
    -- An error is tossed because something has probably gone horribly
    -- wrong if any 'Left' values are present.
    -- VARIK is willing to modify @memberRooms@ such that
    -- @memberRooms@ does not break at this point if any users of this
    -- module would benefit from this change.

-- | @memberSpaces@ fetches a list of the 'Space's of which Matel's user
-- is a member.
--
-- = Arguments
--
-- The first and only argument is the authorisation information of the
-- user of Matel.
--
-- = Output
--
-- The output of @memberSpaces@ is an IO-monadic list of 'Space's of
-- which Matel's user is a member.
--
-- = Exception Handling
--
-- @memberSpaces@ may throw an error and burst into flames.  Feel free
-- to request the removal of this error functionality if this error
-- functionality is found to be inconvenient.
memberSpaces :: Auth
             -- ^ The authorisation information of the Matrix user,
             -- probably Matel's user, whose joined spaces should be
             -- fetched
             -> IO [Space];
memberSpaces = idOrError <.> joinedSpaces;

-- | @memberComms@ outputs a list of the 'Community's --
-- EEUUUAAaaARGH -- of which Matel's user is a member.
--
-- = Arguments
--
-- The only argument is the authorisation information of Matel's user.
--
-- = Output
--
-- The output is an IO-monadic list of 'Community's -- again,
-- EEUUUAAaaARGH -- of which Matel's user is a member.
--
-- = Exception Handling
--
-- @memberComms@ may throw an error and burst into flames.  Feel free to
-- request the removal of this error functionality if this error
-- functionality is found to be inconvenient.
memberComms :: Auth
            -- ^ The authorisation information of Matel's user
            -> IO [Community];
memberComms = idOrError <.> joinedComms;

-- | @idOrError (Right k) == k@.  @idOrError (Left k)@ throws an 'error'
-- whose message is @k@.
idOrError :: Either Stringth a -> a;
idOrError = either (error . T.unpack) id;

-- | @markRead@ is used to mark messages as having been read.
--
-- @markRead@ is currently nonfunctional.
--
-- = Arguments
--
-- The first argument is a representation of the message which is to
-- be marked as "read".  The @messageId@ field of this argument MUST
-- be defined and valid; if the field is not defined and valid, then
-- @markRead@ may be reduced to a small pile of leaf-rolling weevils.
-- But such behaviour is not guaranteed.
--
-- The second argument is the authorisation stuff which has already
-- been documentated 87956 times.
--
-- = Output
--
-- @markRead@ returns 'Nothing' if no problem is encountered.
--
-- However, if a problem is encountered, then @markRead@ 'Just' returns
-- a description of this problem.
markRead :: StdMess
         -- ^ The message which should become "read"
         -> Auth
         -- ^ Authorisation crap
         -> IO (Maybe ErrorCode);
markRead _ _ = error "markRead is unimplemented.";

-- $stuffSend
--
-- This section of the module contains the functions of this module
-- which are responsible for the sending of stuff, e.g., messages.

-- | @send@ is used to send messages to encrypted and unencrypted Matrix
-- rooms.
--
-- = Arguments
--
-- The first argument represents the message which is to be sent to the
-- Matrix room.  If the destination room is encrypted, then an encrypted
-- version of the message which this argument represents is sent.
-- Otherwise, the unencrypted version of this message is sent.
--
-- The second argument represents the Matrix room to which the message
-- is sent.  Only the @roomId@ field must be non-default.
--
-- The third argument is authorisation crap.
--
-- = Output
--
-- If an error is encountered, then 'Just' a description of this error
-- is returned.  Otherwise, 'Nothing' is returned.
send :: StdMess
     -- ^ The message which is to be sent
     -> Room
     -- ^ The room to which the message is sent
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

-- | @roomEncrypt m r@ returns an 'Encrypted' message which can be read
-- by the authenticated members of @r@.
roomEncrypt :: StdMess
            -- ^ The message which should be encrypted
            -> Room
            -- ^ The room for which the message is encrypted
            -> IO (Either ErrorCode Encrypted);
roomEncrypt _ _ = pure $ Left "roomEncrypt is unimplemented.";

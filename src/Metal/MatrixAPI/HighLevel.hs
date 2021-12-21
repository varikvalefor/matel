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

-- | @recentMessagesFrom n rm a@ fetches the @n@ most recent text-based
-- messages from rm, outputting the unencrypted/decrypted messages.
--
-- @recentMessagesFrom@ is currently nonfunctional.
recentMessagesFrom :: Integer
                   -- ^ The number of messages which should be fetched
                   -> Room
                   -- ^ The room from which the messages should be
                   -- fetched
                   -> Auth
                   -- ^ Authorisation crap
                   -> IO [StdMess];
recentMessagesFrom n = fetchEvents n 'b' Def.stdMess;

-- | @earlyMessagesFrom n rm a@ fetches the @n@ earliest text-based
-- messages from rm, outputting the unencrypted/decrypted messages.
--
-- @earlyMessagesFrom@ is currently nonfunctional.
earlyMessagesFrom :: Integer
                  -- ^ The number of messages which should be fetched
                  -> Room
                  -- ^ The room from which messages should be fetched
                  -> Auth
                  -- ^ The authorisation details with which messages are
                  -- fetched
                  -> IO [StdMess];
earlyMessagesFrom n = fetchEvents n 'b' Def.stdMess;

-- | @memberRooms x@ equals a list of all rooms of which Matel's user,
-- whose login information is contained within @x@, is a member.
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

-- | @memberSpaces x@ equals a list of all spaces of which Matel's user,
-- whose login information is contained within @x@, is a member.
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

-- | @memberComms a@ equals a list of all Matrix communities of which
-- Matel's user, whose login information is contained within @a@, is a
-- member.
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

-- | @markRead k a@ marks @k@ as having been read.
--
-- @markRead k a@ returns 'Nothing' if no problem is encountered.
-- However, if a problem is encountered, then markRead k a@ 'Just'
-- returns a description of this problem.
--
-- The @messageId@ field of @k@ must be defined and valid; if this field
-- is not defined and valid, then Metal.MatrixAPI.HighLevel may be
-- reduced to a small pile of leaf-rolling weevils.  But such behaviour
-- is not guaranteed.
--
-- @markRead@ is currently nonfunctional.
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

-- | If the room which @r@ represents is encrypted, then @send e r@
-- sends an encrypted @e@ to @r@.  @send e r@ otherwise sends the plain
-- old @e@ to @r@.
--
-- In both cases, an error message is returned iff something goes _too_
-- horribly wrong.
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
  maybeEncrypt :: IO (Either Stringth (Either StdMess Encrypted))
  maybeEncrypt = getRoomInformation italy a >>= either (return . Left) (Right <.> process)
  blowUp = return . Just . T.unpack
  process dullards = if isEncrypted dullards
                       -- \| These dullards can AT LEAST use
                       -- encryption... allegedly.
                       then Right <$> roomEncrypt event dullards
                       -- \| man yall dullards cant even use encryption
                       -- what a scam
                       -- dang
                       else Left <$> pure event;

-- | @roomEncrypt m r@ returns an 'Encrypted' message which can be read
-- by the authenticated members of @r@.
roomEncrypt :: StdMess
            -- ^ The message which should be encrypted
            -> Room
            -- ^ The room for which the message is encrypted
            -> IO Encrypted;
roomEncrypt = error "roomEncrypt is unimplemented.";

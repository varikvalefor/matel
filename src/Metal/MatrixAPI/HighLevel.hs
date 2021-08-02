 {-# LANGUAGE OverloadedStrings #-}

-- | Metal.MatrixAPI.HighLevel contains functions which use the Matrix
-- API by chaining together relatively low-level functions for the
-- Matrix API.
--
-- This module differs from Metal.MatrixAPI.LowLevel because the
-- functions within this module transparently support encryption and do
-- not explicitly use HTTP queries, whereas the functions of
-- Metal.MatrixAPI.LowLevel generally explicitly use HTTP queries and
-- support only explicit encryption.
module Metal.MatrixAPI.HighLevel where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.Space;
import Control.Monad;
import Metal.Community;
import Metal.Messages.Standard;
import Metal.MatrixAPI.LowLevel;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Either as EE;

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
recentMessagesFrom n rm a = error "recentMessages is unimplemented.";

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
earlyMessagesFrom n rm a = error "recentMessages is unimplemented.";

-- | @memberRooms x@ equals a list of all rooms of which Matel's user,
-- whose login information is contained within @x@, is a member.
memberRooms :: Auth
            -- ^ The information which is used to authenticate Matel's
            -- user
            -> IO [Room];
memberRooms a = joinedRooms a >>= maybeShowRms
  where
  listRoomsMentioned :: Either Stringth [Room] -> IO ([Either Stringth Room])
  listRoomsMentioned = either convS (mapM (flip getRoomInformation a))
    where
    convS :: Stringth -> IO [Either Stringth Room]
    convS = return . return . Left
  --
  maybeShowRms :: Either Stringth [Room] -> IO [Room]
  maybeShowRms = listRoomsMentioned >=> \t ->
    if any EE.isLeft t
      then error $ toString $ justLeft $ head $ filter EE.isLeft t
      -- An error is tossed because something has probably gone horribly
      -- wrong if any 'Left' values are present.
      -- VARIK is willing to modify @memberRooms@ such that
      -- @memberRooms@ does not break at this point if any users of this
      -- module would benefit from this change.
      else return $ map justRight t
  --
  toString :: Stringth -> String
  toString = map (toEnum . fromEnum) . T.unpack;

-- | @memberSpaces x@ equals an IO-monadic list of all spaces of which
-- Matel's user, whose login information is contained within @x@, is a
-- member.
--
-- @memberSpaces@ is currently nonfunctional.
memberSpaces :: Auth -> IO [Space];
memberSpaces a = error "memberSpaces is unimplemented.";

-- | @memberComms a@ equals the IO-monadic list of all Matrix
-- communities of which Matel's user, whose login information is
-- contained within @a@, is a member.
memberComms :: Auth -> IO [Community];
memberComms a = error "memberComms is unimplemented.";

-- | @isSentToRoom g k a@ only if @g@ is sent to Matrix room @k@ from
-- the account which is specified in @a@.
--
-- @isSentToRoom g k a@ equals an IO-monadic @""@ if no problem is
-- encountered.  @isSentToRoom g k a@ otherwise equals an explanation of
-- the problem.
--
-- @isSentToRoom@ is currently nonfunctional.
isSentToRoom :: StdMess
             -- ^ The message which should be sent
             -> Room
             -- ^ The room to which the message should be sent
             -> Auth
             -- ^ Authorisation crap
             -> IO (Maybe ErrorCode);
isSentToRoom ms rm a = error "isSentToRoom is unimplemented.";

-- | @markRead k a@ marks @k@ as having been read.
--
-- @markRead k a@ equals an IO-monadic @""@ if no problem is
-- encountered.  @markRead k a@ otherwise equals an explanation of the
-- problem.
--
-- The @messageId@ field of @k@ must be defined and valid.
--
-- @markRead@ is currently nonfunctional.
markRead :: StdMess
         -- ^ The message which should become "read"
         -> Auth
         -- ^ Authorisation crap
         -> IO (Maybe ErrorCode);
markRead k a = error "markRead is unimplemented.";

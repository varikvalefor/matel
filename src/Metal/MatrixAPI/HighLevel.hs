{- |
 - Module      :  $Header$
 - Description :  $Header$ contains "high-level" functions for Matrix.
 - Copyright   :  (c) Varik Valefor
 - License     :  BSD-3-Clause
 -
 - Maintainer  :  varikvalefor@aol.com
 - Stability   :  unstable
 - Portability :  portable
 -
 - $Header$ contains functions which use the Matrix API by chaining
 - together relatively low-level functions for the Matrix API.
 - -}

 {-# LANGUAGE OverloadedStrings #-}

module Metal.MatrixAPI.HighLevel where
import Metal.Auth;
import Metal.Base;
import Metal.Room;
import Metal.Space;
import Metal.Community;
import Metal.Messages.Standard;
import Metal.MatrixAPI.LowLevel;
import qualified Data.Either as EE;
import qualified Data.ByteString as BS;

-- | @recentMessagesFrom n rm a@ fetches the @n@ most recent text-based
-- messages from rm, outputting the unencrypted/decrypted messages.
--
-- @recentMessagesFrom@ is currently nonfunctional.
recentMessagesFrom :: Integer -> Room -> Auth -> IO [StdMess];
recentMessagesFrom n rm a = error "recentMessages is unimplemented.";

-- | @earlyMessagesFrom n rm a@ fetches the @n@ earliest text-based
-- messages from rm, outputting the unencrypted/decrypted messages.
--
-- @earlyMessagesFrom@ is currently nonfunctional.
earlyMessagesFrom :: Integer -> Room -> Auth -> IO [StdMess];
earlyMessagesFrom n rm a = error "recentMessages is unimplemented.";

-- | @memberRooms x@ equals the IO-monadic list of all rooms of which
-- Matel's user, whose login information is contained within @x@, is a
-- member.
memberRooms :: Auth -> IO [Room];
memberRooms a =
  sendJoinedRooms a >>= \jrOut ->
  if EE.isLeft jrOut
    then error $ toString $ EE.fromLeft "a" jrOut
    else listRoomsMentioned jrOut >>= \getRmOut ->
      if any EE.isLeft getRmOut
        then error $ toString $ EE.fromLeft "a" (getRmOut !! 0)
        else return $ map (\(Right k) -> k) getRmOut
  where
  listRoomsMentioned :: Either Stringth Stringth -> IO ([Either Stringth Room]);
  listRoomsMentioned (Right k) = mapM (flip getRoomInformation a) $ stringthToListRoomIdentifier k
  --
  toString :: BS.ByteString -> String
  toString = map (toEnum . fromEnum) . BS.unpack;

-- | @memberSpaces x@ equals the IO-monadic list of all spaces of which
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

-- | @isSentToRoom g k a@ only if a message whose body is @g@ is sent to
-- Matrix room @k@ from the account which is specified in @a@.
--
-- @isSentToRoom g k a@ equals an IO-monadic @""@ if no problem is
-- encountered.  @isSentToRoom g k a@ otherwise equals an explanation of
-- the problem.
--
-- @isSentToRoom@ is currently nonfunctional.
isSentToRoom :: MessageText -> Identifier -> Auth -> IO ErrorCode;
isSentToRoom ms rm a = error "isSentToRoom is unimplemented.";

-- | @isSentToRoom_file g k a@ only if a message to which the file at
-- @g@ is attached is sent to Matrix room @k@ from the account which is
-- specified in @a@.
--
-- @isSentToRoom_file g k a@ equals an IO-monadic @""@ if no problem is
-- encountered.  @isSentToRoom_file g k a@ otherwise equals an
-- explanation of the problem.
--
-- @isSentToRoom@ is currently nonfunctional.
isSentToRoom_file :: FilePath -> Identifier -> Auth -> IO ErrorCode;
isSentToRoom_file phile romhack a =
  error "isSentToRoom_file is unimplemented.";

-- | @markRead k a@ marks @k@ as having been read.
--
-- @markRead k a@ equals an IO-monadic @""@ if no problem is
-- encountered.  @markRead k a@ otherwise equals an explanation of the
-- problem.
--
-- @markRead@ is currently nonfunctional.
markRead :: Mess a => a -> Auth -> IO ErrorCode;
markRead k a = error "markRead is unimplemented.";

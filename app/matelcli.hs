{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Main
-- Description : Business end of MATELCLI
-- Copyright   : (c) Varik Valefor, 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
-- @matelcli@ is a command-line interface for Matrix which uses
-- Matel's underlying "Metal" infrastructure.
--
-- @matelcli@ is written such that for all text-based features of Matel
-- @k@, @matelcli@ is capable of doing @k@.
--
-- The user-facing documentation/specification of @matelcli@ is
-- available in @matelcli@'s manual page, which is by default located at
-- @[MATEL GIT REPOSITORY DIRECTORY]\/matelcli.1@.  This documentation
-- is only of particular interest to men who wish to modify @matelcli@
-- or understand the inner workings of @matelcli@.
module Main where
import Plegg;
import GetAuth;
import Data.Bool;
import Text.Read;
import Data.Maybe;
import Metal.Base;
import Metal.Room;
import Metal.User;
import Metal.Auth;
import Data.Either;
import System.Exit;
import Metal.Space;
import Metal.Community;
import Control.Concurrent;
import System.Environment;
import Control.Monad ((>=>));
import Metal.Messages.FileInfo;
import Metal.Messages.Standard;
import Metal.EventCommonFields;
import Metal.OftenUsedFunctions;
import qualified Data.Text as T;
import qualified Data.Text.IO as T;
import Metal.MatrixAPI.HighLevel as H;
import qualified Metal.Default as Def;
import qualified Data.ByteString.Lazy as BSL;

-- | Chicken chow mein main...
main :: IO ();
main = ensureSecurity >> doStuff
  where
  ensureSecurity = univac >> plegg
  doStuff = getAuthorisationDetails >>= runWithAuth
  runWithAuth aufFile = getArgs >>= flip determineAction aufFile;

-- | @determineAction@ determines the action which should be taken by
-- @matelcli@, e.g., listing stuff or sending a message.
determineAction :: [String]
                -- ^ This argument is the @matelcli@ command, as read
                -- from @argc@.
                -> Auth
                -- ^ This argument is the authorisation information of
                -- the user of Matel.
                -> IO ();
determineAction [] = error "I never thought that I would have a \
                           \stress-induced heart attack by the age of \
                           \forty, but you're making me rethink some \
                           \things.";
determineAction (command:stuff) = case command of
  "list"       -> list stuff
  "send"       -> Main.send stuff
  "grab"       -> grab stuff
  "login"      -> logIn >=> T.putStrLn
  "markread"   -> mkRead stuff
  "sync"       -> eddySmith stuff >=> T.putStrLn
  "join"       -> runJoin stuff
  "leave"      -> runLeave stuff
  "kick"       -> runKick stuff
  "createroom" -> createRoom' stuff
  "upload"     -> ooplawed stuff
  "ban"        -> blam stuff
  "unban"      -> deblam stuff
  _            -> error "An unrecognised command is input.  \
                  \RTFM, punk.";

-- | @list@ implements the "list" command.
list :: [String]
     -- ^ The first argument specifies the type of the things which
     -- should be listed.  The first element of the list is the only
     -- part of the list which is actually used.
     --
     -- If the first element of this thing is "rooms", then the rooms of
     -- which the specified user is a member are listed.
     --
     -- If the first element of this dingus is "spaces", then the spaces
     -- of which the specified user is a member are listed.
     --
     -- If the first element of the first argument is "communities",
     -- then the communities of which the specified user is a member are
     -- listed.
     --
     -- = "Yo, Why Is this Thing a List?"
     --
     -- Demanding that a singleton list\* is input seems a bit
     -- ridiculous at first glance.  However, handling the extraction of
     -- command line arguments within @list@ implies being able to have
     -- a /somewhat/ short @'determineAction'@, which is nice.
     --
     -- \*Technically, the list /can/ have multiple elements.
     -> Auth
     -- ^ This argument is the authorisation information of the user
     -- whose joined things are listed.  This user is /probably/ also
     -- the user of @matelcli@.
     -> IO ();
list [] = error "You wimps suck.";
list (k:_) = memberXIds >=> mapM_ putStrLn
  where
  possibly f = either (error . T.unpack) (map f)
  memberXIds = case k of
    "rooms"       -> possibly roomId <.> memberRooms
    "communities" -> possibly commId <.> memberComms
    "spaces"      -> possibly spaceId <.> memberSpaces
    _             -> error "The pathologists will be listing your \
                     \injuries if you don't stop inputting crap.";

-- | @send@ implements the "send" command.
send :: [String]
     -- ^ This bit is the ['String']-based command-line arguments of
     -- @matelcli@, e.g., @["text", "!hskonBonfjiIefqLUV:matrix.org"]@.
     --
     -- If this value matches the pattern @["text", foo]@, then a
     -- text-based message whose body is read from the standard is sent
     -- to the Matrix room whose internal Matrix room ID is @foo@.
     --
     -- If this value matches the pattern @["file", filename, foo]@, then
     -- the content of a file is read from the standard input.  This file
     -- is uploaded to Matel's user's homeserver as @k@, and a link which
     -- directs to this file is sent to the Matrix room whose internal ID
     -- is @foo@.
     -> Auth
     -- ^ This argument is the authorisation information.
     -> IO ();
send [] a = error "I need some arguments, fat-ass.";
send [_] a = error "I thought that you were improving.  I now see that \
                   \I was wrong.  Really, I should be mad at myself \
                   \for apparently going insane by having some faith \
                   \in you.";
send (msgtype:k) a = getTarget >>= \t -> H.send t dest a >>= dispError
  where
  getTarget :: IO StdMess
  getTarget = case msgtype of
    "text"     -> (\i -> Def.stdMess {body = i}) <$> T.getContents
    "file"     -> uploadStdinGetID (head k) a >>= \uploadID ->
                  return Def.stdMess {
                    msgType = Attach,
                    body = T.pack $ head k,
                    url = Just $ T.unpack uploadID,
                    fileInfo = Just Def.fileInfo {
                      mimetype = Just "text/plain"
                    }
                  }
    "notice"   -> T.getContents >>= \input ->
                  return Def.stdMess {body = input, msgType = Notice}
    "location" -> T.getContents >>= \input ->
                  return Def.stdMess {
                    msgType = Location,
                    -- \| Using @listToMaybe@ SHOULD be unnecessary, as
                    -- @k@ SHOULD NOT be @null@.  However, using
                    -- @listToMaybe@ implies not needing to manually
                    -- place @head k@ into the 'Maybe' monad.
                    geo_uri = T.pack <$> listToMaybe k,
                    body = input
                  }
    _          -> error "I ought to send you to the garbage disposal, \
                        \shit-tits.  Read the fucking manual."
  --
  dest :: Room
  dest = Def.room {roomId = k !! destIndex}
    where
    diargumentalStuff = ["file", "location"]
    destIndex = bool 0 1 $ msgtype `elem` diargumentalStuff
    -- \^ This bit is necessary because the number of arguments of the
    -- "send file" command is not equal to the number of arguments of
    -- the "send text" and "send notice" commands.

-- | @uploadStdinGetID@ uploads some data which is read from the
-- standard input to a homeserver, returning the URI of the uploaded
-- file if everything goes according to plan.
--
-- = Output
--
-- If everything goes according to plan, then the MXC URI of the
-- uploaded file is returned.
--
-- However, if NOT(EVERYTHING GOES ACCORDING TO PLAN), then
-- @uploadStdinGetID@ probably just bursts into flame.
uploadStdinGetID :: String
                 -- ^ This bit is the name of the file which is
                 -- uploaded.  This name need not be the current name of
                 -- the file.
                 -> Auth
                 -- ^ This argument is the authorisation data of the
                 -- user whose account uploads the file.
                 -> IO Stringth;
uploadStdinGetID p90 = either (error . T.unpack) id <.> uploadThing
  where
  uploadThing off = BSL.getContents >>= \c -> upload c p90 off;

-- | @blam@ bans users... if the proper authorisation information is
-- had.
blam :: [String]
     -- ^ This thing is a 3-list of the non-authorisation-related
     -- arguments which are passed to @ban@.
     --
     -- The first argument is the MXID of the user which should be
     -- banned.
     --
     -- The second argument is the room from which the user is forcibly
     -- removed.
     --
     -- The third argument is the justification for the removal of the
     -- user, e.g., "yo, this dude stole my fuckin' 'nanners."
     -> Auth
     -- ^ This thing is the authorisation information of the account
     -- which is used to ban the /other/ user account.
     -> IO ();
blam (u':r':j:_) = ban u r j >=> maybe (return ()) (error . T.unpack)
  where
  u = Def.user {username = u'}
  r = Def.room {roomId = r'};
blam _ = error "The \"ban\" command demands 3 arguments, tubby.";

-- | @deblam@ un-bans users... if the proper authorisation is had.
deblam :: [String]
       -- ^ This thing is a 2-list of the arguments which are tossed
       -- to @unban@.
       --
       -- The first element of this list is the MXID of the user which
       -- should be un-banned.
       --
       -- The second element of this list is the ID of the room @k@
       -- such that the specified user should no longer be banned from
       -- @k@.
       -> Auth
       -- ^ This argument is the authorisation information which is...
       -- the reader probably "knows the deal".
       -> IO ();
deblam (u':r':_) = unban u r >=> maybe (return ()) (error . T.unpack)
  where
  u = Def.user {username = u'}
  r = Def.room {roomId = r'};
deblam _ = error "The \"unban\" command demands 2 arguments, tubby.";

-- | @grab@ is used to fetch and output the messages of a room.
grab :: [String]
     -- ^ This argument is a 4-list whose elements are as follows:
     --
     -- 1. The number of messages which should be fetched
     --
     -- 2. The word "early" or "recent"
     --
     -- 3. Junk data
     --
     -- 4. The Matrix ID of the Matrix room from which the messages are
     -- fetched
     -> Auth
     -- ^ This bit is the authorisation information of the user account.
     -> IO ();
grab (decino:eeyore:jd:mexico:_) a
  | n < 0 = error "I need a natural number, not garbage."
  | n == 0 = error "Why in the hell would you want to take 0 messages?\
                   \  0 is not a natural number, anyway."
  | otherwise = nabMessages n destination a >>= mapM_ print
  where
  nabMessages = case eeyore of
    "recent" -> recentMessagesFrom
    "early"  -> earlyMessagesFrom
    _        -> error "I'll grab you if you don't grab some sense."
  -- \| This variable refers to the number of messages which should be
  -- fetched.
  n :: Integer
  n = fromMaybe (-42) $ readMaybe decino
  --
  destination :: Room
  destination = Def.room {roomId = mexico};
grab _ _ = error "Repent, motherfucker.";

-- | @mkRead@ marks messages as having been read.
mkRead :: [String]
       -- ^ This argument is a 1-list of the message ID of the message
       -- which should be marked as having been read.
       -> Auth
       -- ^ This bit is the authorisation information which is used to
       -- mark the message as having been read.
       -> IO ();
mkRead [] = error "Someone should knock you upside the head a few \
                  \times, punk.  Dismissed.";
mkRead (eeee:_) = markRead Def.stdMess {boilerplate = boi} >=> dispError
  where
  boi :: EventCommonFields
  boi = Def.eventCommonFields {eventId = eeee};

-- | @dispError@ displays error messages without needlessly feeding
-- lines.
--
-- If @k == Nothing@, then @dispError k@ does nothing.  @dispError k@
-- otherwise runs @error k@.
dispError :: Maybe ErrorCode -> IO ();
dispError = maybe (return ()) (error . T.unpack);

-- | @logIn@ generates authorisation tokens.
--
-- If the authorisation token is generated successfully, then @logIn@
-- adds this authorisation token to @$PATH/.config/matel@, writes the
-- authorisation token to the standard output, and returns this
-- authorisation token.  @logIn@ otherwise 'splodes.
logIn :: Auth
      -- ^ This bit is the authorisation information of the user for
      -- which an authorisation token is generated.
      -> IO T.Text;
logIn = loginPass >=> either busticate addAndDisplay
  where
  addAndDisplay :: T.Text -> IO T.Text
  addAndDisplay toke = configFilePath >>= processPath
    where
    processPath path = T.readFile path >>= writeAndReturn path
    writeAndReturn path phile = writeAppended path phile >> pure phile
    writeAppended path phile = T.writeFile path $ addToken phile toke
  --
  addToken :: T.Text -> T.Text -> T.Text
  addToken phile toke = withNewToken $ withoutOldToken phile
    where
    withoutOldToken = filter (not . beginsWith "authToken: ") . T.lines
    withNewToken = T.unlines . (++ [T.append "authtoken: " toke])
  --
  beginsWith :: T.Text -> T.Text -> Bool
  beginsWith fieldName = (== fieldName) . T.take (T.length fieldName)
  --
  busticate :: T.Text -> IO T.Text
  busticate = error . ("logIn: " ++) . T.unpack;

-- | @eddySmith@ is a command-line-friendly wrapper for @'sync'@.
--
-- = Output
--
-- The output is the verbatim body of the homeserver's response to the
-- "sync" request.
eddySmith :: [String]
          -- ^ This argument determines the "since" value which is
          -- attached to the "sync" request.  If this argument is @[]@,
          -- then no "since" value is attached.  If this argument is
          -- some other thing, then the first element of this argument
          -- is used as the "since" value of the sync request.
          -> Auth
          -- ^ This argument, as ever, is the boring boilerplate
          -- authorisation information.
          -> IO Stringth;
eddySmith t = either (error . T.unpack) id <.> sync (listToMaybe t);

-- | @runJoin@ is a relatively command-line-friendly wrapper for
-- @'join'@.
runJoin :: [String]
        -- ^ This argument contains the command-line arguments of the
        -- @join@ command.  This thing should be a 1-list or a 4-list.
        --
        -- If thids argument is a 1-list, then this 1-list simply
        -- contains the internal Matrix ID of the room which should be
        -- joined.
        --
        -- If some user has sent an invitation which permits joining
        -- the room which should be joined, then this argument should be
        -- a 4-list whose elements are as follows:
        --
        -- 1. The internal matrix ID of the room which the user should
        -- join
        --
        -- 2. The username of the user which sends the invite to the
        --    user which should join the room
        --
        -- 3. The state key of the invitation which the user receives
        --
        -- 4. The signature of the invite which is sent
        -> Auth
        -- ^ This thing, as ever, is the standard bullshit authorisation
        -- crap.
        -> IO ();
runJoin [] = error "Idiot!  How am I to join an unspecified room for \
                   \you?  My strength is simplicity.  I can't work \
                   \with this shit.";
runJoin t = join room inviteInfo >=> dispError
  where
  room :: Room
  room = Def.room {roomId = t !! 0}
  --
  inviteInfo :: Maybe (User, String, String)
  inviteInfo = case length t of
    4 -> Just (Def.user {username = t !! 1}, t !! 2, t !! 3)
    1 -> Nothing
    _ -> error "You have managed to completely disregard the \
         \information which is specified in my manual page by \
         \inputting a weird number of arguments, which is \
         \actually not terribly impressive... but is still a bit \
         \irritating.";

-- | @runLeave@ is a relatively high-level interface for the @'leave'@
-- command.
--
-- = Processing
--
-- An error is encountered iff an error is thrown.
runLeave :: [String]
         -- ^ The first element of this list is the room ID of the room
         -- which the user should leave.  This element is the only
         -- element which must be contained within this argument this
         -- argument is only of type ['String'] and not 'String' because
         -- just making this thing use a ['String'] facilitates writing
         -- a clean @'determineAction'@.
         -> Auth
         -- ^ This argument is the authorisation information which is
         -- used to actually leave the specified room.
         -> IO ();
runLeave (x:_) = leave Def.room {roomId = x} >=> dispError;
runLeave _ = error "You'd best leave... or stop giving me \
                   \nothing but bullshit.";

-- | @runKick@ is a relatively command-line-friendly interface for the
-- @'kick'@ command.
--
-- = Processing
--
-- An error is encountered iff an error is thrown.
runKick :: [String]
        -- ^ The first 3 elements of this list are the room ID of the
        -- room from which the user should be removed, and the reason
        -- for the removal of this user.  If the third element equals
        -- @""@, then no reason is supplied.
        -> Auth
        -- ^ This argument is /still/ the same old authorisation stuff.
        -- user
        -> IO ();
runKick (uninat:cell:remo:_) = kick user room remo >=> dispError
  where
  user = Def.user {username = uninat}
  room = Def.room {roomId = cell};
runKick _ = error "I'll kick YOUR ass if you don't start giving \
                  \me some actual directions."

-- | @createRoom'@ creates new Matrix rooms.
--
-- = Processing
--
-- If the creation of this room is a success, then the internal Matrix
-- ID of this Matrix room is written to the standard output.  If
-- something violently falls apart, then an error is thrown.
createRoom' :: [String]
            -- ^ This argument is a 3-list whose elements, are as
            -- follows:
            --
            -- 1. The display name of the Matrix room which should be
            --    created
            --
            -- 2. The topic message of the Matrix room which should be
            --    created
            --
            -- 3. Depending upon whether the new room should be private
            --    or public, "private" or "public", respectively
            -> Auth
            -- ^ This argument is a very imaginative representation of
            -- a turtle... or just the same old authorisation bullshit.
            --
            -- "Every day is exactly the same.  There is no love here,
            -- and there is no pain."
            -> IO ();
createRoom' [] = error "";
-- The above error message is a particularly smart-ass error message.
--
-- The error message in question might not be particularly enlightening,
-- but reading the manual page should yield the desired enlightenment.
createRoom' [_] = error "Your one-word demands are starting to piss me \
                        \off.";
createRoom' [_,_] = error "Should I just assume that you want to make \
                          \all of your communications public?";
createRoom' (nm:tpc:pbl:_) = createRoom rm pbl >=> display
  where
  toMaybe :: String -> Maybe Stringth
  toMaybe k = bool (Just $ T.pack k) Nothing $ null k
  --
  rm = Def.room {roomName = toMaybe nm, topic = toMaybe tpc}
  display = either (error . T.unpack) (putStrLn . roomId);

-- | @messToHumanReadable@ is roughly equivalent to @show@.  However,
-- VARIK finds that the readability of @messToHumanReadable@'s output
-- is greater than the readability of @show@'s output.
messToHumanReadable :: StdMess
                    -- ^ This argument is the message whose
                    -- "human-readable" representation is desired.
                    -> String;
messToHumanReadable k = timespec ++ name ++ sendIntro ++ show (body k)
  where
  timespec = "At " ++ show (origin_server_ts $ boilerplate k) ++ ", "
  name = username $ sender $ boilerplate k
  sendIntro = " sends the following " ++ show (msgType k) ++ ": ";

-- | @ooplawed@ uploads a file which is read from the standard input to
-- the homeserver of Matel's user.
--
-- = Uploading Unencrypted Files
--
-- Users of @ooplawed@ should note that @ooplawed@ uploads UNENCRYPTED
-- files.  When the @protocol@ is HTTPS, @ooplawed@ /is/ TLS-protected.
-- However, @ooplawed@ does /not/ support "true" end-to-end encryption.
ooplawed :: [String]
         -- ^ The first element of this list is the desired name of the
         -- uploaded file.  No other elements of this list are actually
         -- used.
         -> Auth
         -- ^ Authorisation information... ZZZ...
         -> IO ();
ooplawed (f:_) = uploadStdinGetID f >=> T.putStrLn;

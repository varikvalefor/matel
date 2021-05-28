module Backend.BasicStuff where
  type RoomID = String;
  type HomeServerID = String;
  type UserName = String;
  type CommandArgs = [String];
  type Command = (CommandType, CommandArgs);
  
  data CommandType = GoToRoom deriving (Print, Show, Eq, Read);

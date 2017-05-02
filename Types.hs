module Types where
  import Data.Maybe

  data ClientName = StudentName | TeacherName
  type ChannelName = String
  type StudentName = String
  type TeacherName = String

  data Post = Post { questionID :: Int,
                     subject :: String,
                     question :: String,
                     response :: Maybe String,
                     respondedBy :: Maybe ClientName }

  data Message = Question {studentName :: StudentName, q :: Post }
               | Response {teachername :: TeacherName, r :: Post }
               | Command String
               | Broadcast { channelName :: ChannelName, clientName :: ClientName, post :: Post}

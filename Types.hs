module Types where
  import Data.Maybe
  import Post

  data ClientName = StudentName | TeacherName
  type ChannelName = String
  type StudentName = String
  type TeacherName = String
  type QuestionID = Int

  data Message = Question {studentName :: StudentName, q :: Post }
               | Response {teachername :: TeacherName, r :: Post }
               | Command String
               | Notice String
               | Broadcast { channelName :: ChannelName, clientName :: ClientName, post :: Post}

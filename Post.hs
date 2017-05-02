module Post where
  data Post = Post { questionID :: QuestionID,
                     subject :: String,
                     question :: String,
                     response :: Maybe String,
                     respondedBy :: Maybe ClientName }

  newPost :: Int -> String -> String -> STM Post
  newPost qid subject question = return Post {
                                        questionID = qid,
                                        subject = subject,
                                        question = question,
                                        response = Nothing,
                                        respondedBy = Nothing
                                        }

  newPostWithResponse :: Int -> String -> String -> String -> ClientName -> STM Post
  newPostWithResponse qid s q r cn = return Post {
                                                  questionID = qid,
                                                  subject = subject,
                                                  question = question,
                                                  response = Just r,
                                                  respondedBy = Just cn
                                          }

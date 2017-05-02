{-# LANGUAGE LambdaCase #-} --https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XLambdaCase
{-# LANGUAGE RecordWildCards #-} --https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XRecordWildCards

module Server where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception        (finally, mask)
import           Control.Monad
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           System.IO
import           Text.Printf              (hPrintf, printf)

import Channel
import Client
import Types

data Server = Server
    { serverChannels :: TVar (Map ChannelName Channel)
    , serverStudents  :: TVar (Map StudentName Client)
    , serverTeachers :: TVar (Map TeacherName Client)
    , questions :: TVar (Map QuestionID Post)
    }

newServer :: IO Server
newServer = atomically $ do
    server <- Server <$> newTVar M.empty <*> newTVar M.empty <*> newTVar M.empty <*> newTVar M.empty
    addChannel server IntialChannel
    return server

IntialChannel :: ChannelName
IntialChannel = "Lista Usuarios"

-- Agregamos un nuevo canal de listas al servidor.. la lista de preguntas cambia???
addChannel :: Server -> ChannelName -> STM ()
addChannel Server{..} name = newChannel name >>= modifyTVar serverChannels . M.insert name

lookupChannel :: Server -> ChannelName -> STM (Maybe Channel)
lookupChannel Server{..} name = M.lookup name <$> readTVar serverChannels

lookupClient :: Server -> ClientName -> STM (Maybe Client)
lookupClient Server{..} name = M.lookup name <$> readTVar serverClients

lookupPost :: Server -> QuestionID -> STM (Maybe Post)
lookupPost Server{..} questionId = M.lookup questionId <$> readTVar questions

addResponse :: Server -> QuestionId -> ClientName -> String -> STM()
addResponse Server{..} questionId name response = do
  questionsMap <- readTVar questions
  _question <- M.lookup questionId <$> questionsMap
  newPost <- newPostWithResponse questionId _question.subject _question.question response name
  return modifyTVar questions . case M.lookup questionsMap questionsMap of
                                   Nothing -> questionsMap
                                   Just e  -> M.insert newPost e (M.delete questionsMap questionsMap)

addQuestion :: Server -> String -> String -> STM()
addQuestion Server{..} subject question = do
  questionID <- getNextQuestionID
  return newPost subject question >>= modifyTVar questions . M.insert questionID

getNextQuestionID :: Int
getNextQuestionID =  do
  questionsMap <- readTVar questions
  return (getMaxFromMap questionsMap) + 1

getMaxFromMap :: Map -> Int
getMaxFromMap _map = map fst (filter isBiggest sorted)
    where sorted = List.sortBy (\(k1, v1) (k2, v2) -> k2 `compare` k1) $ Map.toList _map
          maxV = fst ( head sorted )
          isBiggest (key, value) = value == maxV

-- sacar un cliente del servidor.
removeClient :: Server -> Client -> IO ()
removeClient Server{..} client@Client{..} = atomically $ do
    clientLeaveChannel LeaveReasonDisconnected client
    modifyTVar' serverClients ( M.delete clientName )


-- handler para responder a los requests de un cliente.
handleClient :: Handle -> Server -> IO ()
handleClient handle server@Server{..} = do
    -- Swallow carriage returns sent by telnet clients.
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    readName
  where
    readName :: IO ()
    readName = do
        hPutStrLn handle "Enter your username:"
        name <- hGetLine handle
        if null name
            then readName
            else mask $ \restore -> tryAddClient server name handle >>= \case
                Nothing -> restore $ do
                    _ <- hPrintf handle "The name '%s' is already logged" name
                    readName
                Just client ->
                    restore (runClient server client)
                        `finally` removeClient server client


-- Try to add a client to the server; fail if the requested name is taken.
-- server@Server{..} is set up from the
tryAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
tryAddClient server@Server{..} name handle = atomically $ do
    clients <- readTVar serverClients
    if M.member name clients
        then return Nothing
        else do
            client <- newClient name handle
            notify client ConnectionMessage

            Just generalChat <- lookupChannel server defaultChannelName    -- Siempre se asume el canal por default.. que podria hacerse si se desea conectarse a una lista de preguntas distinta???.
            _ <- clientJoinChannel JoinReasonConnected client generalChat

            writeTVar serverClients $ M.insert name client clients
            return (Just client)

ConnectionMessage :: String
ConnectionMessage = unlines
    [ "Bienvenido al servidor de lista de usuarios"
    , "/questions                 - List the questions."
    , "/ask subject question      - Ask a question in the current questions list"
    , "/respond id response       - Respond a question from the questions in a channel"
    , "/whoami                    - list your username"
    , "/quit                      - quit the server"
    ]

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
    receiveThread
    return ()
  where
    receiveThread :: IO ()
    receiveThread = forever $ do
        msg <- hGetLine clientHandle
        atomically $ sendMessage client (Command msg)

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage _ client (Notice msg)    = sendBytes client ("*** " ++ msg) >> return True
handleMessage _ client (Broadcast chName clName msg)
    | clName == clientName client = return True  -- Ignorar mensajes de propio cliente...
    | otherwise                 = sendBytes client (printf "[%s] %s: %s" chName clName msg) >> return True
handleMessage server client@Client{..} (Command msg) = case words msg of

    "/ask":subject:what:[]     -> ask server client subject what                     >> return True
    "/a":subject:what:[]       -> ask server client subject what                     >> return True

    "/respond":id:respond:[]      -> respondQuestion server client id respond        >> return True
    "/r":id:respond:[]            -> respondQuestion server client id respond        >> return True

    "/whoami":[]               -> whoami client                                      >> return True

    "/quit":[]                 ->                                                       return False
    "/q"   :[]                 ->                                                       return False

    ('/':_):_                  -> sendBytes client ("Unrecognized command: " ++ msg) >> return True
    _                          -> atomically (clientBroadcastToChannel client msg)   >> return True



respond :: Server -> Client -> Int -> String -> IO ()
respond server@Server{..} client@Client{..} questionID response = atomically $
    ....

ask :: Server -> Client -> String -> String -> IO()
    .....

whoami :: Client -> IO ()
whoami client@Client{..} = atomically $ do
    msgSuffix <- maybe (", not in any list.")
                       ((" in list " ++) . channelName)
                       <$> clientGetChan client
    notify client $ printf "You are %s%s" clientName msgSuffix

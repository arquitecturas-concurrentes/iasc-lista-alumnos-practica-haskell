module Client where
  import Types
  import Control.Concurrent.STM
  import System.IO              (Handle, hPutStrLn)

  import Types

  data Client = Client
      { clientName          :: ClientName -- Store the client name that could be a Student or Teacher Name
      , clientHandle        :: Handle
      , clientPersonalChan  :: TChan Message -- Este es el canal de comunicacion entre el cliente y el servidor, permite que pueda despues desconectarse del canal de preguntas.
      , clientChan          :: TVar (Maybe (Channel, TChan Message))
      }

  newClient :: ClientName -> Handle -> STM Client
  newClient clientName handle = do
    personalChan  <- newTChan
    broadcastChan <- newTVar Nothing
    kicked        <- newTVar Nothing
    return Client
        { clientName          = name
        , clientHandle        = handle
        , clientPersonalChan  = personalChan
        , clientChan          = broadcastChan
        }

  clientGetChan :: Client -> STM (Maybe Channel)
  clientGetChan = (fmap . fmap) fst . readTVar . clientChan

  clientGetBroadChan :: Client -> STM (Maybe (TChan Message))
  clientGetBroadChan = (fmap . fmap) snd . readTVar . clientChan

  clientReadMessage :: Client -> STM Message
  clientReadMessage client = readPersonal `orElse` readBroadcast
    where
      -- Leer del channel entre el cliente y el servidor...
      readPersonal :: STM Message
      readPersonal = readTChan (clientPersonalChan client)

      -- Leer del canal de broadcast, este canal es el mismo
      readBroadcast :: STM Message
      readBroadcast = clientGetBroadChan client >>= maybe retry readTChan

  clientJoinChannel :: JoinReason -> Client -> Channel -> STM (Maybe Channel)
  clientJoinChannel reason client@Client{..} newChan = readTVar clientChan >>= \case
      -- No esta en un canal, unirse a uno nuevo
      Nothing -> do
          clientJoinChannel'
          return Nothing
      -- ya esta en un canal conectarse a otro canal de preguntas.
      Just (curChan, _)
          | channelName curChan == channelName newChan -> do
              notify client "You're already in that channel."
              return Nothing
          | otherwise -> do
              clientJoinChannel'
              chanRemoveClient LeaveReasonLeft curChan clientName
              return (Just curChan)
    where
      clientJoinChannel' :: STM ()
      clientJoinChannel' = do
          notify client $ "Joined channel: " ++ channelName newChan

          -- Se duplica el canal de broadcast para reutilizar el mismo a traves de los distintos clientes.
          chanAddClient reason newChan clientName
          dupTChan (channelBroadcastChan newChan) >>=
              writeTVar clientChan . Just . (newChan,)

  -- Dejar el canal de preguntas
  clientLeaveChannel :: LeaveReason -> Client -> STM ()
  clientLeaveChannel reason client@Client{..} = readTVar clientChan >>= \case
      -- No esta en el canal. No se hace nada
      Nothing -> notify client "You're not in a channel."
      -- Si esta en el canal, salir de ahi. Que sucede con el canal de broadcast???
      Just (curChan, _) -> do
          notify client $ "Left channel: " ++ channelName curChan
          chanRemoveClient reason curChan clientName
          writeTVar clientChan Nothing

  -- hacer broadcast de un mensaje de un cliente desde su actual canal de preguntas. Si el cliente no esta en el canal no se hace nada.
  clientBroadcastToChannel :: Client -> String -> STM ()
  clientBroadcastToChannel client@Client{..} msg =
      clientGetChan client >>= maybe (return ()) (\c -> chanBroadcast c clientName msg)

  -- Enviar un mensaje al cliente
  sendMessage :: Client -> Message -> STM ()
  sendMessage Client{..} = writeTChan clientPersonalChan

  -- Notificar a un cliente
  notify :: Client -> String -> STM ()
  notify client = sendMessage client . Notice

  -- Enviar bytes en forma raw a un cliente.
  sendBytes :: Client -> String -> IO ()
  sendBytes Client{..} = hPutStrLn clientHandle

module Client where
  import Types
  import Control.Concurrent.STM
  import System.IO              (Handle, hPutStrLn)

  import Types

  data Client = Client
      { clientName          :: ClientName -- Store the client name that could be a Student or Teacher Name
      , clientHandle        :: Handle
      , clientKicked        :: TVar (Maybe (String,String))
      , clientPersonalChan  :: TChan Message
      -- If the client is in a Channel, it needs to keep two data: the Channel
      -- itself (so it's possible to later leave the channel), and its dup'd
      -- broadcast TChan. Store these concomitant values in a tuple.
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
        , clientKicked        = kicked
        , clientPersonalChan  = personalChan
        , clientChan          = broadcastChan
        }

  -- get the client's current channel.
  clientGetChan :: Client -> STM (Maybe Channel)
  clientGetChan = (fmap . fmap) fst . readTVar . clientChan

  -- get the client's current broadcast channel.
  clientGetBroadChan :: Client -> STM (Maybe (TChan Message))
  clientGetBroadChan = (fmap . fmap) snd . readTVar . clientChan

  clientReadMessage :: Client -> STM Message
  clientReadMessage client = readPersonal `orElse` readBroadcast
    where
      -- Read from the
      readPersonal :: STM Message
      readPersonal = readTChan (clientPersonalChan client)

      -- Read from the Broadcast channel
      readBroadcast :: STM Message
      readBroadcast = clientGetBroadChan client >>= maybe retry readTChan

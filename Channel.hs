{-# LANGUAGE RecordWildCards #-} --https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XRecordWildCards

module Channel where

import           Control.Applicative
import           Control.Concurrent.STM
import           Data.Set               (Set)
import qualified Data.Set               as S

import Types

-- Razon por la cual se fue un usuario de un canal
data LeaveReason
    = LeaveReasonLeft
    | LeaveReasonDisconnected

data JoinReason
    = JoinReasonJoined
    | JoinReasonConnected

-- Canal de preguntas
data Channel = Channel
    { channelName          :: ChannelName
    , channelClients       :: TVar (Set ClientName)
    , channelBroadcastChan :: TChan Message
    }

newChannel :: ChannelName -> STM Channel
newChannel name = Channel name <$> newTVar S.empty <*> newBroadcastTChan

-- Remove a ClientName from this Channel's clients. Notify the channel of the
-- client leaving or disconnecting.
chanRemoveClient :: LeaveReason -> Channel -> ClientName -> STM ()
chanRemoveClient LeaveReasonLeft         = chanRemoveClient' chanNotifyHasLeft
chanRemoveClient LeaveReasonDisconnected = chanRemoveClient' chanNotifyHasDisconnected

chanRemoveClient' :: (Channel -> ClientName -> STM ()) -> Channel -> ClientName -> STM ()
chanRemoveClient' notifyAction chan@Channel{..} name = do
    notifyAction chan name
    modifyTVar channelClients . S.delete $ name

-- Add a ClientName to this Channel's clients.
chanAddClient :: JoinReason -> Channel -> ClientName -> STM ()
chanAddClient JoinReasonJoined    = chanAddClient' chanNotifyHasJoined
chanAddClient JoinReasonConnected = chanAddClient' chanNotifyHasConnected

chanAddClient' :: (Channel -> ClientName -> STM ()) -> Channel -> ClientName -> STM ()
chanAddClient' notifyAction chan@Channel{..} name = do
    notifyAction chan name
    modifyTVar channelClients . S.insert $ name

-- ### Notificaciones ### --
chanNotifyHasLeft :: Channel -> ClientName -> STM ()
chanNotifyHasLeft chan name = chanNotify chan (name ++ " has left the list.")

chanNotifyHasDisconnected :: Channel -> ClientName -> STM ()
chanNotifyHasDisconnected chan name = chanNotify chan (name ++ " has disconnected.")

chanNotifyHasJoined :: Channel -> ClientName -> STM ()
chanNotifyHasJoined chan name = chanNotify chan (name ++ " has joined the list.")

chanNotifyHasConnected :: Channel -> ClientName -> STM ()
chanNotifyHasConnected chan name = chanNotify chan (name ++ " has connected.")

-- ### Mensajes ### --
chanMessage :: Channel -> Message -> STM ()
chanMessage = writeTChan . channelBroadcastChan

chanBroadcast :: Channel -> ClientName -> String -> STM ()
chanBroadcast chan@Channel{..} clientName msg = chanMessage chan (Broadcast channelName clientName msg)

chanNotify :: Channel -> String -> STM ()
chanNotify chan = chanMessage chan . Notice

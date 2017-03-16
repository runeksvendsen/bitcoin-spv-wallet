{-# LANGUAGE RecordWildCards #-}
module Bitcoin.SPV.Wallet.Server
( spawnWallet
)
where

import LibPrelude
import Bitcoin.SPV.Wallet.Interface     (Interface, initInterface)
import Network.Haskoin.Wallet           (Config(..))
import Network.Haskoin.Wallet.Server    (runSPVServerWithContext)
import Network.Haskoin.Wallet.Internals (Notif(..))

import qualified Network.Haskoin.Crypto         as HC
import qualified System.ZMQ4                    as ZMQ
import qualified Control.Monad.Logger           as Log
import qualified Control.Monad.Trans.Resource   as Resource
import qualified Data.Aeson                     as JSON
import qualified Control.Concurrent             as Con
import qualified Control.Monad                  as M
import qualified Control.Exception              as Except
import qualified Control.Retry                  as Re
import qualified Control.Monad.Catch            as Catch


spawnWallet :: Config
            -> ZMQ.Context
            -> HC.XPubKey
            -> (Notif -> IO ())
            -> IO Interface
spawnWallet conf ctx pk notifHandler = do
    -- Server
    putStrLn "Starting server..."
    _ <- Con.forkIO $ runWallet conf ctx
    -- Notify thread
    putStrLn "Starting notification thread..."
                      -- Give the wallet 10 seconds to boot up
    _ <- Con.forkIO $ notifyThread conf ctx notifHandler
    initInterface conf ctx pk

-- |Run haskoin-wallet using the specified ZeroMQ Context,
--  and log to stderr.
runWallet :: Config -> ZMQ.Context -> IO ()
runWallet cfg ctx = run $ runSPVServerWithContext cfg ctx
    where run           = Resource.runResourceT . runLogging
          runLogging    = Log.runStderrLoggingT . Log.filterLogger logFilter
          logFilter _ l = l >= configLogLevel cfg

-- |Connect to notify socket, subscribe to new blocks,
--  and execute the supplied handler for each new block as it arrives.
notifyThread :: Config -> ZMQ.Context -> (Notif -> IO ()) -> IO ()
notifyThread Config{..} ctx handler = Re.recovering
    -- Keep trying to connect every 1s a maximum of 1,000 times
    (Re.constantDelay 1000000 <> Re.limitRetries 1000)
    [ \stat -> Catch.Handler $ \(e :: ZMQ.ZMQError) -> return True ] $ \retryStat ->
        ZMQ.withSocket ctx ZMQ.Sub $ \sock -> do
            ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
            putStrLn "DEBUG: Trying to connect to notify socket."
            ZMQ.connect sock configBindNotif
            ZMQ.subscribe sock "[block]"
            putStrLn "NOTIFY: Connected. Subscribed to new blocks."
            M.forever $ do
                [_,m] <- ZMQ.receiveMulti sock
                notif <- either failOnErr return $ JSON.eitherDecode (cs m)
                handler notif
  where
    failOnErr = fail . ("NOTIFY: ERROR: recv failed: " ++)

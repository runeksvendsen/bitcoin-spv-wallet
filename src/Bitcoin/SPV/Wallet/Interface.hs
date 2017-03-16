{-# LANGUAGE RecordWildCards #-}
module Bitcoin.SPV.Wallet.Interface
(
    initInterface
,   Interface(..)
)
where

import LibPrelude
import Bitcoin.SPV.Wallet.Types
import Bitcoin.SPV.Wallet.Cmd     ( runCmd
                                  , cmdGetStatus, cmdListKeys
                                  , cmdImportTx, cmdBlockInfo )

import qualified Bitcoin.SPV.Wallet.Init    as Init
import qualified Network.Haskoin.Crypto     as HC
import qualified Network.Haskoin.Node.STM   as Node
import qualified System.ZMQ4                as ZMQ


data Interface = Interface
  { nodeStatus  :: IO Node.NodeStatus
  , networkSync :: IO Bool
  , listKeys    :: IO [JsonAddr]
  , importTx    :: BitcoinTx -> IO JsonTx
  , blockInfo   :: BlockHash -> IO (Maybe BlockInfo)
  }

initInterface :: Config -> ZMQ.Context -> HC.XPubKey -> IO Interface
initInterface cfg ctx pubkey =
    Init.initAccount cfg ctx pubkey >>=
    mkInterface cfg ctx

mkInterface :: Config -> ZMQ.Context -> Text -> IO Interface
mkInterface cfg ctx accountName =
    return $ Interface
        nodeStatus'
        (syncedWithNetwork nodeStatus')
        (runCmd (cfg,ctx) $ cmdListKeys accountName)
        (runCmd (cfg,ctx) . cmdImportTx accountName)
        (runCmd (cfg,ctx) . cmdBlockInfo)
  where
    nodeStatus' = runCmd (cfg,ctx) $ cmdGetStatus


syncedWithNetwork :: IO Node.NodeStatus -> IO Bool
syncedWithNetwork nodeStatus =
    nodeStatus >>= \ns ->
        return $ Node.nodeStatusBestHeaderHeight ns == Node.nodeStatusNetworkHeight ns


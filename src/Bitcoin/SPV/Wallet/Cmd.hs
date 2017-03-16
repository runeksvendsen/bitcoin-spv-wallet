{-# LANGUAGE RecordWildCards #-}
module Bitcoin.SPV.Wallet.Cmd where

import              LibPrelude
import              Network.Haskoin.Wallet
import qualified Network.Haskoin.Wallet.Internals as HI
import qualified Control.Monad.Reader as R
import qualified Network.Haskoin.Crypto      as HC
import qualified Network.Haskoin.Block       as HB
import qualified Network.Haskoin.Node.STM    as Node
import qualified System.ZMQ4                 as ZMQ
import qualified Data.Aeson                  as JSON




cmdBlockInfo :: HB.BlockHash -> WalletM (Maybe HI.BlockInfo)
cmdBlockInfo bh = listToMaybe <$>
    ( cmdSend (GetBlockInfoR [bh]) >>= getResOrFail "cmdBlockInfo" )

cmdNewAccount :: NewAccount -> WalletM JsonAccount
cmdNewAccount na =
    cmdSend (PostAccountsR na) >>= getResOrFail "cmdNewAccount"

cmdListAccounts :: WalletM [JsonAccount]
cmdListAccounts =
    cmdSend (GetAccountsR listRequestAll) >>= getResOrFail "cmdListAccounts"

cmdListAddresses :: Text -> Word32 -> WalletM [JsonAddr]
cmdListAddresses accountName minConf =
    cmdSend
        (GetAddressesR accountName AddressExternal minConf True listRequestAll) >>=
    getResOrFail "cmdListAddresses"

cmdListKeys :: Text -> WalletM [JsonAddr]
cmdListKeys name = cmdListAddresses name 0

cmdGetStatus :: WalletM Node.NodeStatus
cmdGetStatus =
    cmdSend (PostNodeR NodeActionStatus) >>= getResOrFail "cmdGetStatus"

cmdImportTx :: Text -> BitcoinTx -> WalletM JsonTx
cmdImportTx name tx =
    cmdSend cmd >>= getResOrFail "cmdImportTx"
        where cmd = PostTxsR name Nothing (ImportTx tx)



getResOrFail :: Monad m => String -> Maybe a -> m a
getResOrFail name = maybe (error $ "ERROR: " ++ name ++ ": no response.") return


type WalletM = R.ReaderT (Config, ZMQ.Context) IO

runCmd :: (Config, ZMQ.Context) -> WalletM a -> IO a
runCmd = flip R.runReaderT

cmdSend :: (JSON.FromJSON a, JSON.ToJSON a)
        => WalletRequest
        -> WalletM (Maybe a)
cmdSend req = do
    cfg <- R.asks fst
    ctx <- R.asks snd
    R.liftIO $ sendCmdOrFail cfg ctx req


sendCmdOrFail :: (JSON.FromJSON a, JSON.ToJSON a)
              => Config -> ZMQ.Context -> WalletRequest -> IO (Maybe a)
sendCmdOrFail cfg ctx cmd =
    _sendCmd cfg ctx cmd >>=
    either error return >>=
    \res -> case res of
        ResponseError e -> error $ "ERROR: Send cmd, ResponseError: " ++ cs e
        ResponseValid r -> return r

_sendCmd :: (JSON.FromJSON a, JSON.ToJSON a)
        => Config
        -> ZMQ.Context
        -> WalletRequest
        -> IO (Either String (WalletResponse a))
_sendCmd Config{..} ctx cmd =
    ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
        ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
        ZMQ.connect sock configBind
        ZMQ.send sock [] (cs $ JSON.encode cmd)
        JSON.eitherDecode . cs <$> ZMQ.receive sock

mkNewAccount :: HC.XPubKey -> NewAccount
mkNewAccount xpub = NewAccount
    { newAccountName     = "default"
    , newAccountType     = AccountRegular
    , newAccountMnemonic = Nothing
    , newAccountMaster   = Nothing
    , newAccountDeriv    = Nothing  -- Use root key
    , newAccountKeys     = [xpub]
    , newAccountReadOnly = True
    }

listRequestAll :: ListRequest
listRequestAll = ListRequest
        { listOffset  = 0
        , listLimit   = maxBound
        , listReverse = True
        }
module Bitcoin.SPV.Wallet.Init where

import              LibPrelude
import              Bitcoin.SPV.Wallet.Cmd
import              Network.Haskoin.Wallet
import qualified    Network.Haskoin.Crypto      as HC
import qualified    System.ZMQ4                 as ZMQ


type AccountName = Text

initAccount :: Config -> ZMQ.Context -> HC.XPubKey -> IO AccountName
initAccount cfg ctx pubkey = runCmd (cfg,ctx) $ do
    accountList <- cmdListAccounts
    if null accountList then
             createInitialWalletAccount
        else checkAccount accountList
    where
        createInitialWalletAccount =
            cmdNewAccount (mkNewAccount pubkey) >>
            liftIO (putStrLn "WALLET_INIT: INFO: Initialized wallet account.") >>
            return (newAccountName $ mkNewAccount pubkey)
        checkAccount :: [JsonAccount] -> WalletM AccountName
        checkAccount acl = do
            when (length acl /= 1) $
                error "WALLET_INIT: ERROR: There should be only one account in the wallet."
            checkPubKey (head acl)
            return (jsonAccountName $ head acl)
        checkPubKey acc = when (accPubKey acc /= pubkey) $
            error $ "WALLET_INIT: ERROR: Unknown pubkey in wallet: " ++ show (accPubKey acc)
        accPubKey acc = head (jsonAccountKeys acc)


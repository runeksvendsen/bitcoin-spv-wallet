{-# LANGUAGE OverloadedStrings #-}
module Bitcoin.SPV.Wallet.Config
(
  mkConfig
, defaultConfig
, BitcoinNet(..)
, Config(..)
)
where

import           Bitcoin.SPV.Wallet.NodeList     (btcNodes, BTCNode)
import           Network.Haskoin.Wallet           (Config(..), AddressType(..),
                                                   OutputFormat(..), SPVMode(..))
import qualified Control.Monad.Logger           as Log
import qualified Data.HashMap.Strict            as HM
import qualified Database.Persist.Sqlite        as DB
import qualified Data.Text as T


data BitcoinNet = Prodnet | Testnet deriving Eq

toStr :: BitcoinNet -> T.Text
toStr Prodnet = "prodnet"
toStr Testnet = "testnet"

databaseConf :: DB.SqliteConf   --TODO: config
databaseConf = DB.SqliteConf "/Users/rune/IdeaProjects/bitcoin-clearing-server/db2" 1

cmdSocket :: String
cmdSocket = "inproc://cmd"

notifSocket :: String
notifSocket = "inproc://notif"

-- TODO: Custom nodes
mkConfig ::
       BitcoinNet
    -> DB.SqliteConf
--     -> [BTCNode]
    -> Config
mkConfig btcNet dbConf =
    defaultConfig
        { configTestnet  = btcNet == Testnet
        , configDatabase = HM.fromList [ ( toStr btcNet, dbConf ) ]
--         , configBTCNodes = HM.fromList [ ( toStr btcNet, nodes ) ]
        }



defaultConfig :: Config
defaultConfig = Config
    { configCount         = 100
    -- ^ Output size of commands
    , configMinConf       = 6
    -- ^ Minimum number of confirmations
    , configSignTx        = True
    -- ^ Sign transactions
    , configFee           = 50000
    -- ^ Fee to pay per 1000 bytes when creating new transactions
    , configRcptFee       = False
    -- ^ Recipient pays fee (dangerous, no config file setting)
    , configAddrType      = AddressExternal
    -- ^ Return internal instead of external addresses
    , configOffline       = False
    -- ^ Display the balance including offline transactions
    , configReversePaging = True
    -- ^ Use reverse paging for displaying addresses and transactions
    , configPath          = Nothing
    -- ^ Derivation path when creating account
    , configFormat        = OutputNormal
    -- ^ How to format the command-line results
    , configConnect       = cmdSocket
    -- ^ ZeroMQ socket to connect to (location of the server)
    , configConnectNotif  = notifSocket
    -- ^ ZeroMQ socket to connect for notifications
    , configDetach        = False
    -- ^ Detach server when launched from command-line
    , configFile          = ""
    -- ^ Configuration file
    , configTestnet       = False
    -- ^ Use Testnet3 network
    , configDir           = ""
    -- ^ Working directory
    , configBind          = cmdSocket
    -- ^ Bind address for the ZeroMQ socket
    , configBindNotif     = notifSocket
    -- ^ Bind address for ZeroMQ notifications
    , configBTCNodes      = HM.fromList [ ( "prodnet", btcNodes ) ]
    -- ^ Trusted Bitcoin full nodes to connect to
    , configMode          = SPVOnline
    -- ^ Operation mode of the SPV node.
    , configBloomFP       = 0.00001
    -- ^ False positive rate for the bloom filter.
    , configDatabase      = HM.fromList [ ( "prodnet", databaseConf ) ]
    -- ^ Database configuration
    , configLogFile       = ""
    -- ^ Log file
    , configPidFile       = ""
    -- ^ PID File
    , configLogLevel      = Log.LevelWarn
    -- ^ Log level
    , configVerbose       = True
    -- ^ Verbose
    , configServerKey     = Nothing
    -- ^ Server key for authentication and encryption (server config)
    , configServerKeyPub  = Nothing
    -- ^ Server public key for authentication and encryption (client config)
    , configClientKey     = Nothing
    -- ^ Client key for authentication and encryption (client config)
    , configClientKeyPub  = Nothing
    -- ^ Client public key for authentication and encryption
    }

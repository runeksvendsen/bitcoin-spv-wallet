module Bitcoin.SPV.Wallet.Test where

import qualified Network.Haskoin.Crypto   as HC
import           Data.Maybe                 (fromJust)


testPubKey :: HC.XPubKey
testPubKey = fromJust $ HC.xPubImport
    "xpub6CWiJoiwxPQni3DFbrQNHWq8kwrL2J1HuBN7zm4xKPCZRmEshc7Dojz4zMah7E4o2GEEbD6HgfG7sQid186Fw9x9akMNKw2mu1PjqacTJB2"

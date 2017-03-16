module Bitcoin.SPV.Wallet.NodeList
( btcNodes
, BTCNode(..)
)
where

import Network.Haskoin.Wallet.Internals (BTCNode(..))


btcNodes :: [BTCNode]
btcNodes =
    [ BTCNode "bitcoin-seed.runeks.me"          8333
    , BTCNode "dnsseed.bluematt.me"             8333
    , BTCNode "dnsseed.bitcoin.dashjr.org"      8333
    , BTCNode "dnsseed.bluematt.me"             8333
    , BTCNode "seed.bitcoinstats.com"           8333
    , BTCNode "seed.bitcoin.jonasschnelli.ch"   8333
    , BTCNode "seed.bitcoin.sipa.be"            8333
    , BTCNode "seed.bitnodes.io"                8333
    , BTCNode "seed.btcc.com"                   8333
    ]

module Bitcoin.SPV.Wallet.Types
(
  HI.BlockInfo(..)
, HB.BlockHash
, Config, JsonAddr, JsonTx
)
where

import           Network.Haskoin.Wallet               (Config, JsonAddr, JsonTx)
import qualified Network.Haskoin.Wallet.Internals   as HI
import qualified Network.Haskoin.Block              as HB



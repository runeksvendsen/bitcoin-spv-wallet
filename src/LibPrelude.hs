module LibPrelude
( Text
, cs
, BitcoinTx
, Word32
, module X
, R.liftIO
)
where

import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Network.Haskoin.Transaction        as HT
import Data.Word (Word32)
import Data.Maybe as X
import Control.Monad as X
import Data.Monoid   as X
import qualified Control.Monad.Reader as R
type BitcoinTx = HT.Tx
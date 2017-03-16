module Bitcoin.SPV.Wallet.Keys where

import           Network.Haskoin.Wallet       (JsonAddr(..))
import qualified Network.Haskoin.Crypto     as HC
import           Data.Word
import           Data.Maybe                   (isJust, catMaybes, listToMaybe)

indexByPubKey :: HC.PubKeyC -> [JsonAddr] -> Maybe Word32
indexByPubKey pubkey =
    handleResult . catMaybes . filter isJust . map (match pubkey)
    where
        handleResult [i] = Just i
        handleResult [] = Nothing
        handleResult l@(_:_:_) = error $ "WALLET_INDEX: ERROR: Multiple pubkeys match: " ++ show l
        match _         JsonAddr{ jsonAddrKey = Nothing } = error "WALLET_INDEX: ERROR: No PubKeyC in JsonAddr"
        match pkToMatch JsonAddr{ jsonAddrKey = Just pk, jsonAddrIndex = i } =
            if pk == pkToMatch then
                    Just i
                else
                    Nothing


-- |Return PubKeyC at index 0, if it's contained in the '[JsonAddr]', otherwise Nothing.
--  Throws error if 'JsonAddr' doesn't contain a 'PubKeyC' in 'jsonAddrKey'.
indexZeroPubKey :: [JsonAddr] -> Maybe HC.PubKeyC
indexZeroPubKey =
    getPubKey . listToMaybe . filter ((== 0) . jsonAddrIndex)
        where getPubKey (Just jsonAddr) = failOnMissingPubKey $ jsonAddrKey (jsonAddr :: JsonAddr)
              getPubKey Nothing = Nothing
              failOnMissingPubKey (Just pk) = Just (pk :: HC.PubKeyC)
              failOnMissingPubKey Nothing   = error "No 'PubKeyC' in 'JsonAddr' at 'jsonAddrKey'"

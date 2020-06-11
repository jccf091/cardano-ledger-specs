{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( ledgerSpendOneUTxO,
    ledgerRegisterOneStakeKey,
    ledgerDeRegisterOneStakeKey,
  )
where

import Control.State.Transition.Extended (TRC (..), applySTS)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (pattern KeyHashObj)
import Shelley.Spec.Ledger.Delegation.Certificates
  ( pattern DeRegKey,
    pattern RegKey,
  )
import Shelley.Spec.Ledger.Keys
  ( KeyRole (..),
    asWitness,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    emptyDPState,
    genesisCoins,
    genesisId,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), emptyPPPUpdates)
import Shelley.Spec.Ledger.STS.Ledger (pattern LedgerEnv)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (..), pattern Tx)
import Shelley.Spec.Ledger.TxData
  ( pattern DCertDeleg,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
    pattern Wdrl,
  )
import Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( DCert,
    DPState,
    KeyPair,
    LEDGER,
    LedgerEnv,
    Tx,
    TxBody,
    TxOut,
    UTxOState,
  )
import Test.Shelley.Spec.Ledger.Examples
  ( aliceAddr,
    alicePay,
    ppsEx1,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( mkKeyPair',
    runShelleyBase,
  )

coins :: Integer -> [TxOut]
coins n = fmap (\_ -> TxOut aliceAddr (Coin $ 100)) [0 .. n]

utxoState :: Integer -> UTxOState
utxoState n =
  UTxOState
    (genesisCoins (coins n))
    (Coin 0)
    (Coin 0)
    emptyPPPUpdates

ppsBench :: PParams
ppsBench =
  ppsEx1
    { _minUTxOValue = 10,
      _keyDeposit = Coin 0,
      _minfeeA = 0,
      _minfeeB = 0,
      _maxTxSize = 1000000000
    }

ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 ppsBench (AccountState 0 0)

testLEDGER ::
  (UTxOState, DPState) ->
  Tx ->
  LedgerEnv ->
  Bool
testLEDGER initSt tx env = do
  let st = runShelleyBase $ applySTS @LEDGER (TRC (env, initSt, tx))
  case st of
    Right _ -> True
    Left e -> error $ show e

txbSpendOneUTxO :: TxBody
txbSpendOneUTxO =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 10), TxOut aliceAddr (89)])
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txSpendOneUTxO :: Tx
txSpendOneUTxO =
  Tx
    txbSpendOneUTxO
    mempty
      { addrWits = makeWitnessesVKey (hashTxBody txbSpendOneUTxO) [asWitness alicePay]
      }
    SNothing

ledgerSpendOneUTxO :: Integer -> Bool
ledgerSpendOneUTxO n = testLEDGER (utxoState n, emptyDPState) txSpendOneUTxO ledgerEnv

--
-- Register a stake keys when there are a lot of registered stake keys
--

-- Create n-many stake key pairs
stakeKeys :: Word64 -> [KeyPair 'Staking]
stakeKeys n = fmap (\w -> mkKeyPair' (w, 0, 0, 0, 0)) [1 .. n]

firstStakeKey :: KeyPair 'Staking
firstStakeKey = head $ stakeKeys 1

-- Create n-many stake key registration certificates
stakeKeyRegistrations :: [KeyPair 'Staking] -> StrictSeq DCert
stakeKeyRegistrations keys =
  StrictSeq.fromList $
    fmap (DCertDeleg . RegKey . (KeyHashObj . hashKey . vKey)) keys

-- Create a transaction body that registers n stake credentials.
-- It spends the genesis coin given by the index ix.
txbRegStakeKeys :: Natural -> StrictSeq DCert -> TxBody
txbRegStakeKeys ix regCerts =
  TxBody
    (Set.fromList [TxIn genesisId ix])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 100)])
    -- Fees and Deposits are set to zero
    regCerts
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

makeSimpleTx :: TxBody -> [KeyPair 'Witness] -> Tx
makeSimpleTx body keys =
  Tx
    body
    mempty {addrWits = makeWitnessesVKey (hashTxBody body) keys}
    SNothing

-- Create a transaction that registers n stake credentials.
txRegStakeKeys :: Natural -> [KeyPair 'Staking] -> Tx
txRegStakeKeys ix keys =
  makeSimpleTx
    (txbRegStakeKeys ix $ stakeKeyRegistrations keys)
    [asWitness alicePay]

-- Create a ledger state that has n registered stake credentials.
-- The keys are seeded with (1, 0, 0, 0, 0) to (n, 0, 0, 0, 0)
-- It is pre-populated with 2 genesis coins.
ledgerStateWithNregisteredKeys :: Word64 -> (UTxOState, DPState)
ledgerStateWithNregisteredKeys n = do
  let st =
        runShelleyBase $
          applySTS @LEDGER
            ( TRC
                ( ledgerEnv,
                  (utxoState 1, emptyDPState),
                  txRegStakeKeys 0 (stakeKeys n)
                )
            )
  case st of
    Right st' -> st'
    Left e -> error $ show e

-- Create a ledger state that has n registers one stake credential,
-- and register a new stake key seeded with (0, 0, 0, 0, 0).
ledgerRegisterOneStakeKey :: Word64 -> Bool
ledgerRegisterOneStakeKey n =
  testLEDGER
    (ledgerStateWithNregisteredKeys n)
    (txRegStakeKeys 1 [mkKeyPair' (0, 0, 0, 0, 0)])
    ledgerEnv

-- Create a transaction body that de-registers ONE stake credential,
-- corresponding to the key seeded with (1, 0, 0, 0, 0)
txbDeRegStakeKey :: TxBody
txbDeRegStakeKey =
  TxBody
    (Set.fromList [TxIn genesisId 1])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 100)])
    -- Fees and Deposits are set to zero
    ( StrictSeq.singleton $
        (DCertDeleg . DeRegKey . (KeyHashObj . hashKey . vKey)) firstStakeKey
    )
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that deregisters a stake credentials.
-- It spends the genesis coin indexed by 1.
txDeRegStakeKey :: Tx
txDeRegStakeKey =
  makeSimpleTx
    txbDeRegStakeKey
    [asWitness alicePay, asWitness firstStakeKey]

-- Create a ledger state that has n registers one stake credential,
-- and degregister one of the keys.
ledgerDeRegisterOneStakeKey :: Word64 -> Bool
ledgerDeRegisterOneStakeKey n =
  testLEDGER
    (ledgerStateWithNregisteredKeys n)
    txDeRegStakeKey
    ledgerEnv

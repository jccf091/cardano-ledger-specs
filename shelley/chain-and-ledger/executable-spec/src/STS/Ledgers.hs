{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Ledgers
  ( LEDGERS
  ) where

import           Control.Monad            (foldM)

import           LedgerState
import           PParams
import           Slot
import           UTxO

import           Control.State.Transition

import           STS.Ledger

data LEDGERS

instance STS LEDGERS where
  type State LEDGERS = LedgerState
  type Signal LEDGERS = [Tx]
  type Environment LEDGERS = (Slot, PParams)
  data PredicateFailure LEDGERS = LedgerFailure (PredicateFailure
                                                 LEDGER)
                                  deriving (Show, Eq)
  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

ledgersTransition :: TransitionRule LEDGERS
ledgersTransition = do
  TRC ((slot, pp), ls, txwits) <- judgmentContext
  let (u, dw) = (_utxoState ls, _delegationState ls)
  (u'', dw'') <-
    foldM
      (\(u', dw') (ix, tx) ->
         trans @LEDGER $ TRC ((slot, ix, pp), (u', dw'), tx))
      (u, dw) $
    zip [0 ..] txwits
  pure $
    LedgerState u'' dw'' (_txSlotIx ls)

instance Embed LEDGER LEDGERS where
  wrapFailed = LedgerFailure

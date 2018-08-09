{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Types used for pure transaction processing (aka Toil).

module Pos.Chain.Txp.Toil.Types
       ( Utxo
       , UtxoLookup
       , UtxoModifier
       , formatUtxo
       , utxoF
       , utxoToModifier
       , utxoToLookup
       , GenesisUtxo (..)
       , _GenesisUtxo

       , StakesView (..)
       , svStakes
       , svTotal

       , TxFee(..)
       , MemPool (..)
       , mpLocalTxs
       , mpSize
       , TxMap
       , UndoMap
       , AddrCoinMap
       , applyUtxoModToAddrCoinMap
       ) where

import           Universum

import           Control.Lens           (makeLenses, makePrisms, makeWrapped)
import           Data.Default           (Default, def)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as M (lookup, member, toList)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, later)
import           Serokell.Util.Text     (mapBuilderJson)

import           Pos.Core               (Address, Coin, StakeholderId,
                                         unsafeAddCoin, unsafeSubCoin)
import           Pos.Core.Txp           (TxAux, TxId, TxIn, TxOutAux (..),
                                         TxUndo, _TxOut)
import qualified Pos.Util.Modifier      as MM

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map TxIn TxOutAux

-- | Type of function to look up an entry in 'Utxo'.
type UtxoLookup = TxIn -> Maybe TxOutAux

-- | All modifications (additions and deletions) to be applied to 'Utxo'.
type UtxoModifier = MM.MapModifier TxIn TxOutAux


newtype GenesisUtxo = GenesisUtxo { unGenesisUtxo :: Utxo} deriving (Show)

makePrisms  ''GenesisUtxo
makeWrapped ''GenesisUtxo

----------------------------------------------------------------------------
-- Fee
----------------------------------------------------------------------------

-- | tx.fee = sum(tx.in) - sum (tx.out)
newtype TxFee = TxFee Coin
    deriving (Show, Eq, Ord, Generic, Buildable)

----------------------------------------------------------------------------
-- StakesView
----------------------------------------------------------------------------

data StakesView = StakesView
    { _svStakes :: !(HashMap StakeholderId Coin)
    , _svTotal  :: !(Maybe Coin)
    }



----------------------------------------------------------------------------
-- MemPool
----------------------------------------------------------------------------

type TxMap = HashMap TxId TxAux

data MemPool = MemPool
    { _mpLocalTxs :: !TxMap
      -- | Number of transactions in the memory pool.
    , _mpSize     :: !Int
    }

makeLenses ''MemPool

instance Default MemPool where
    def =
        MemPool
        { _mpLocalTxs = mempty
        , _mpSize     = 0
        }

----------------------------------------------------------------------------
-- UndoMap and AddrCoinsMap
----------------------------------------------------------------------------

type UndoMap = HashMap TxId TxUndo
type AddrCoinMap = HashMap Address Coin

data TestSumType = TestDataConstructor String | SecondTest AnotherType



data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(TVar Bool)
    -- ^ If this flag is `True`, then workers should stop.
    }

makeLenses ''ShutdownContext

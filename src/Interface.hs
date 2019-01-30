{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interface 
    ( module Interface
    , Price(..)
    , Vol(..)
    , OrderSide(..)
    , Quote(..)
    , QuoteBook(..)
    ) where

import Data.Hashable
import Market.Types ( Price(..)
                    , Vol(..)
                    , OrderSide(..)
                    , Quote(..)
                    , QuoteBook(..)
                    )

---------------------------------------
newtype ClientOID = COID Int deriving (Show, Eq, Num, Hashable)

data FillEv price vol
  = FillEv
    { fSide  :: OrderSide
    , fPrice :: Price price   -- the price that was actually used
    , fVol   :: Vol   vol     -- the volume executed in this fill
    , fmCOID :: Maybe ClientOID
    }
    deriving (Show, Eq)

data TradingEv price vol quoteTail counter
    = PlaceEv   (Maybe ClientOID)
    | CancelEv  (Maybe ClientOID)
    | DoneEv    (Maybe ClientOID)
    | FillsEv   [FillEv price vol]
    | BookEv    (QuoteBook price vol quoteTail counter)
    deriving (Show, Eq)

data Action price vol
    = PlaceLimit
        { aSide  :: OrderSide
        , aPrice :: Price price
        , aVol   :: Vol   vol
        , amCOID :: Maybe ClientOID }
    | CancelLimit
        { aCOID  :: ClientOID }
    deriving (Show, Eq)

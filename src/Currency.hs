{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Currency where 

import Money
import           ClassyPrelude.Yesod
import           Data.Aeson                      (Value (String))
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Types


import Data.Word




import Data.Ratio
import GHC.TypeLits
import Money.Aeson
import Money
import qualified Data.Text as T

newtype USD = USD
  { unUSD :: Dense "USD" }
  deriving (Eq, Show, Read, Num, Ord, Generic)

instance PersistField USD where 
    toPersistValue (USD rationalNum) = PersistRational $ someDenseAmount $ toSomeDense rationalNum 
    fromPersistValue (PersistRational rationalNum) = 
      maybe (Left $ "Couldn't convert a TestUSD from persist" ++ (T.pack $ show rationalNum))
            Right (Just $ USD $  dense' rationalNum)

instance PersistFieldSql USD where 
    sqlType _  = SqlNumeric 10 2

newtype TestUSD = TestUSD
  { unTestUSD :: Dense "USD" }
  deriving (Eq, Show, Read, Num, Ord, Generic)

instance PersistField TestUSD where 
    toPersistValue (TestUSD rationalNum) = PersistRational $ someDenseAmount $ toSomeDense rationalNum 
    fromPersistValue (PersistRational rationalNum) = 
      maybe (Left $ "Couldn't convert a TestUSD from persist" ++ (T.pack $ show rationalNum))
            (Right) (Just $ TestUSD $  dense' rationalNum)

instance PersistFieldSql TestUSD where 
    sqlType _  = SqlNumeric 5 2

-- derivePersistField "USD"

newtype NGN = NGN
  { unNGN :: Dense "NGN" }
  deriving (Eq, Show, Read, Ord, Generic)

derivePersistField "NGN"
-- deriving PersistField via Rational

data Currency = USD' (Dense "USD") | NGN' (Dense "NGN")
    deriving (Eq, Show, Read, Ord, Generic)

derivePersistField "Currency"
-- *Util> dense (341 % 100) :: Maybe (Dense "USD")
-- Just (Dense "USD" 341%100)

-- https://hackage.haskell.org/package/safe-money-0.9.1/docs/Money.html
 
a = dense' (0%1) :: Dense "USD"

aDenseDollar = dense' (1%1) :: Dense "USD"
dollarWrapped = USD aDenseDollar


-- class PersistField a where
--     toPersistValue :: a -> PersistValue
--     fromPersistValue :: PersistValue -> Either T.Text a

-- data PersistValue
--     = PersistText Text
--     | PersistByteString ByteString
--     | PersistInt64 Int64
--     | PersistDouble Double
--     | PersistRational Rational
--     | PersistBool Bool
--     | PersistDay Day
--     | PersistTimeOfDay TimeOfDay
--     | PersistUTCTime UTCTime
--     | PersistNull
--     | PersistList [PersistValue]
--     | PersistMap [(Text, PersistValue)]
--     | PersistObjectId ByteString
--     -- ^ Intended especially for MongoDB backend
--     | PersistArray [PersistValue]
--     -- ^ Intended especially for PostgreSQL backend for text arrays
--     | PersistLiteral_ LiteralType ByteString
--     -- ^ This constructor is used to specify some raw literal value for the
--     -- backend. The 'LiteralType' value specifies how the value should be
--     -- escaped. This can be used to make special, custom types avaialable
--     -- in the back end.
--     --
--     -- @since 2.12.0.0
--     deriving (Show, Read, Eq, Ord)
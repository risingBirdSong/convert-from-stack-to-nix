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


module Model where

import           ClassyPrelude.Yesod
import           Data.Aeson                      (Value (String))
import           Database.Persist.Quasi
import           Database.Persist.Types.Email 
import           Database.Persist.Types.Password
import Money
import Currency
import Database.Persist.Class
import Database.Persist.Sql
import qualified Data.ByteString.Char8 as B8   
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
-- import Data.UUID.V4
import Util.UUID
import Types.CustomerAccount

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migrateAll"
  ]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

instance ToJSON (Entity Tag) where
  toJSON (Entity _ Tag {..}) = String tagName

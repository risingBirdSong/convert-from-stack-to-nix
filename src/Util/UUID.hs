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

module Util.UUID where 

import           Database.Persist.Quasi

import           ClassyPrelude.Yesod
import           Database.Persist.Types.Email 
import           Database.Persist.Types.Password
import Money
import Currency

import Database.Persist.Class
import Database.Persist.Sql
import qualified Data.ByteString.Char8 as B8   
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID



instance PersistField UUID.UUID where
  toPersistValue u = PersistDbSpecific . B8.pack . UUID.toString $ u
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID.UUID where
  sqlType _ = SqlOther "uuid"


instance PathPiece UUID.UUID where
  toPathPiece = UUID.toText
  fromPathPiece = UUID.fromText

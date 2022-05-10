{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.RegisterLoginCustomerAccount where

import Control.Monad.Except (ExceptT, throwError)
import Data.Aeson -- why isnt this module recognized
import           Database.Persist.Extended
import           Import                    hiding (FormResult)
import           Web.Forma.Extended
import Money

import           Blaze.ByteString.Builder           
import           Blaze.ByteString.Builder.Char.Utf8 
import           Control.Concurrent                 
-- import           Control.Monad                     
import           Data.Monoid                        
import           Network.Wai                        
import           Yesod.Core 
import           Validation.Email
import           Database.Persist.Types.Email.Internal
import           Data.Maybe
import qualified Data.UUID.V4 as UUID
import Types.CustomerAccount
import Currency
import Data.Ratio
import Database.Persist.Types.Password.Internal


-- import Control.Monad.Cont (MonadIO(liftIO))


type RegisterFields = '["newCustomer", "username", "email","password"]
data Register = Register
-- the order matters and aligns with the order in the applicatives in registerForm
  { registerUsername :: Text,
    registerEmail :: Email,
    registerPassword :: Text
  }
  deriving (Show)

registerForm :: FormParser RegisterFields Text Handler Register
registerForm =
  subParser #newCustomer (Register
    <$> field #username (notEmpty >=> uniqueUsername)
    <*> field #email (notEmpty >=> validEmail >=> uniqueEmail) -- oh thats cool, 
    <*> field #password notEmpty) -- so this is a shaw256 now.. verify this is a password
    -- https://economic-ecosystem.slack.com/archives/C036E6P9GUX/p1648869507972559


-- why does this type sig need rankNTypes {-# LANGUAGE RankNTypes #-}
registerLogic :: Register -> DB (Entity CustomerAccount)
registerLogic Register {..} = do
        uuid <- liftIO $ UUID.nextRandom
        now <- liftIO getCurrentTime
        let startingBalanceDense = dense' (0 % 1) :: (Dense "USD")
        let passWordType = Password registerPassword
        let customerAccount = CustomerAccount uuid registerUsername passWordType now now (USD startingBalanceDense) 
        insertEntity customerAccount
        





type LoginFields = '["username" , "password"]
data Login = Login
-- the order matters and aligns with the order in the applicatives in LoginForm
  { loginUsername :: Text,
    loginPassword :: Text
  }
  deriving (Show)

loginForm :: FormParser LoginFields Text Handler Login
loginForm = Login
    <$> field #username (notEmpty)
    <*> field #password notEmpty -- so this is a shaw256 now.. verify this is a password
    -- https://economic-ecosystem.slack.com/archives/C036E6P9GUX/p1648869507972559


data LoginResults = LoginSuccess {
  crsaToken :: Text
} | LoginFail | LoginNotFound
    deriving (Show, Eq, Generic)

-- why does this type sig need rankNTypes {-# LANGUAGE RankNTypes #-}
loginLogic :: Login -> DB (Maybe (Entity CustomerAccount))
loginLogic Login {..} = selectFirst [CustomerAccountPrimaryUsername ==. loginUsername] []

instance ToJSON LoginResults
instance FromJSON LoginResults
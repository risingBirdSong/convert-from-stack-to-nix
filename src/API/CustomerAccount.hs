{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}




module API.CustomerAccount where 
import Data.UUID.V4
import Data.UUID
import Data.Aeson
import GHC.Generics
import Model
import           Import   
import Database.Persist.Types.Email.Internal
import Data.Aeson.Casing
import Currency
import Money

import Database.Persist.Types.Password.Internal

usdToDense :: USD -> Dense "USD"
usdToDense (USD thedense) = thedense

data CustomerAccountUser = CustomerAccountUser {
    customerAccountUserId :: UUID
} deriving (Show, Generic)

instance ToJSON CustomerAccountUser
instance FromJSON CustomerAccountUser

instance ToJSON USD
instance FromJSON USD


data CustomerBasicAPI = CustomerBasicAPI {
      crsaBankAccount :: UUID
    , crsaUsername :: Text
    , crsaPassword :: Password
    , crsaBalance :: USD
    -- , crsaToken :: Text -- we were returning this token upon registration but no, it belongs with login
    , crsaCreatedAt :: UTCTime
    , crsaUpdatedAt :: UTCTime
    , crsaMessage :: Text
} deriving (Show, Generic)

instance ToJSON CustomerBasicAPI where
        toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelCase .  drop (length ("crsa" :: Text)) }

instance FromJSON CustomerBasicAPI where
        parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelCase .  drop (length ("crsa" :: Text)) }

data CustomerRegisterResultAPI = CRRSuccess CustomerBasicAPI
    deriving (Show, Generic)

instance ToJSON CustomerRegisterResultAPI
instance FromJSON CustomerRegisterResultAPI

data CustomerAccountRegister = CustomerAccountRegister {
    customerAccount :: CustomerBasicAPI
} deriving (Show, Generic)

data BalanceInquiryReq = BalanceInquiryReq {
    balanceInquiryBankAccount :: UUID 
} deriving (Show, Generic)

instance ToJSON BalanceInquiryReq
instance FromJSON BalanceInquiryReq

data BalanceInquiryRes = BalanceInquiryRes {
    balanceInquiryAmount :: Text 
} | BalanceInquiryCustomerNotFound  deriving (Show, Generic)

instance ToJSON BalanceInquiryRes
instance FromJSON BalanceInquiryRes

data DepositReq = DepositReq {
    depositBankAccount :: UUID , 
    depositAmount :: Text
} deriving (Show, Generic)

instance ToJSON DepositReq
instance FromJSON DepositReq

data DepositRes = DepositResSuccess Text | DepositResParseFail | DepositResCustomerNotFound
    deriving (Show, Generic)

instance ToJSON DepositRes
instance FromJSON DepositRes

data SupportPost = SupportPost {
        supportPostBankAccount :: UUID ,
        supportPostText :: Text
} deriving (Show, Generic)

instance ToJSON SupportPost
instance FromJSON SupportPost

data SupportRes = SupportRes {
        supportResText :: Text
} | SupportResNotFound
    deriving (Show, Generic)

instance ToJSON SupportRes
instance FromJSON SupportRes

data WithdrawPost = WithdrawPost {
        withdrawPostBankAccount :: UUID ,
        withdrawPostAmount :: Text
} deriving (Show, Generic)

instance ToJSON WithdrawPost
instance FromJSON WithdrawPost

data WithdrawRes = WithdrawResSuccess {
        withdrawResAmount :: Text
} | WithdrawResInsufficientFunds | WithdrawResParseAmountFail 
    deriving (Show, Generic)

instance ToJSON WithdrawRes
instance FromJSON WithdrawRes

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}

module Handler.CustomerAccount where

-- what? intellisense is working?! expecting crash in 3,2,1...

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
import           Model.RegisterLoginCustomerAccount       
import API.CustomerAccount   
import Data.Ratio
import Currency
import Data.Maybe
import Yesod.Core.Types
import Model.LoginUser
import Database.Persist.Types.Password.Internal
-- import Data.Time (getCurrentTime)
-- import GHC.Base (undefined)


-- mytodo turn this into a 'Model' 

-- mytodo move this to a different folder, it doesnt belong in Handler
addTokenToHeader customerAccountId = do 
      token <- customerAccountIdToToken customerAccountId
      addHeader "token" token


-- mytodo this can be factored out more
postCustomerAccountRegisterR :: Handler CustomerRegisterResultAPI 
postCustomerAccountRegisterR =
      withFormType registerForm $ \Register {..} -> do
      Entity customerAccountId customerAccount <- runDB $ registerLogic (Register {..})
      encoded <- registerCustomerAccount customerAccountId customerAccount
      pure $ encoded

-- mytodo smelly code smell, get rid of Value and factor out the long block of code
postOneTimeCustomerAccountFundingRequest :: Handler Value 
postOneTimeCustomerAccountFundingRequest = do 
      CustomerAccountUser {..} <- requireCheckJsonBody 
      mybCustomerAccount <- runDB $ getBy $ UniqueCustomerAccountBankAcount customerAccountUserId
      case mybCustomerAccount of
        Nothing -> returnJson $ object ["results" .= ("that customer account wasnt found" :: Text)]
        Just (Entity custaccId customerAccount) -> do 
          mybOneTime <- runDB $ selectFirst [OneTimeCustomerAccountFundingCustomerId ==. custaccId] []
          case mybOneTime of 
            Nothing -> do
              let oneTimeFunding = USD $ (dense' (500%1) :: (Dense "USD"))
              now <- liftIO getCurrentTime
              void $ runDB $ do 
                void $ update custaccId [CustomerAccountPrimaryBalance +=. oneTimeFunding]
                void $ insert $ OneTimeCustomerAccountFunding custaccId (Just now)
              returnJson $ object ["results" .= ("your account has been debited a one time 500 dollar bonus!" :: Text)] 
            Just oneTime -> returnJson $ object ["results" .= ("you've already used this bonus" :: Text)]

testing :: Handler ()
testing = do 
  let newValue = TestUSD $ (dense' (10 % 1) :: (Dense "USD"))
  mybSomeDatas :: [Entity SomeData] <- runDB $ selectList [] []
  forM_ mybSomeDatas \(Entity sdid _) -> do 
      void $ runDB $ update sdid [SomeDataBalance +=. newValue]
      pure ()



type  BankAccount = CustomerAccountId
-- | Encode a 'User' with a JWT authentication token.
registerCustomerAccount :: BankAccount -> CustomerAccount -> Handler CustomerRegisterResultAPI
registerCustomerAccount customerAccountId CustomerAccount {..} = do
  token <- customerAccountIdToToken customerAccountId
  liftIO $ print "token from registerCustomerAccount"
  liftIO $ print token
  -- myTodo this below is a pretty cluttered, maybe some recordwildcards could clean this up?
  let registerMessage = ("thanks for signing up! please check your email to complete the registration" :: Text) 
  pure $ CRRSuccess $ CustomerBasicAPI customerAccountBankAcount customerAccountPrimaryUsername customerAccountPrimaryPassword customerAccountPrimaryBalance customerAccountCreatedAt customerAccountUpdatedAt registerMessage

postLoginCustomerAccount :: Handler (LoginResults)
postLoginCustomerAccount =
  withFormType loginForm $ \Login {..} -> do
  mLogin <- runDB $ loginLogic (Login {..})
  case mLogin of 
    Nothing -> pure LoginNotFound
    Just enCa@(Entity caId ca) -> do
      let storedPassword = customerAccountPrimaryPassword ca
      token <- customerAccountIdToToken caId
      if ((Password loginPassword) == storedPassword) then (pure $ LoginSuccess token) else (pure LoginFail)

postCustomerAccountR :: Handler CustomerBasicAPI 
postCustomerAccountR = do 
  CustomerAccountUser {..} <- requireCheckJsonBody 
  mCustomerAccount <- runDB $ selectFirst [CustomerAccountBankAcount ==. customerAccountUserId] []
  -- mCustomerAccount :: ()
  case mCustomerAccount of
    Nothing -> notFound
    Just (Entity caid CustomerAccount {..}) -> do 
      token <- customerAccountIdToToken caid
      pure $ CustomerBasicAPI customerAccountBankAcount customerAccountPrimaryUsername customerAccountPrimaryPassword customerAccountPrimaryBalance customerAccountCreatedAt customerAccountUpdatedAt ("daf" :: Text)
  
postBalanceInquiry :: Handler BalanceInquiryRes
postBalanceInquiry = do
  BalanceInquiryReq {..} <- requireCheckJsonBody
  -- myTodo i need to make something like a get500
  mCustomer <- runDB $ selectFirst [CustomerAccountBankAcount ==. balanceInquiryBankAccount] []
  case mCustomer of
    Nothing -> pure BalanceInquiryCustomerNotFound
    Just (Entity caid CustomerAccount {..}) -> do
      let makeDecimal = denseToDecimal defaultDecimalConf Round (usdToDense customerAccountPrimaryBalance)
      pure $ BalanceInquiryRes makeDecimal

postDeposit :: Handler DepositRes
postDeposit = do
  DepositReq {..} <- requireCheckJsonBody
  let convertToDense :: Maybe (Dense "USD") = denseFromDecimal defaultDecimalConf depositAmount
  mBankAccount <- runDB $ selectFirst [CustomerAccountBankAcount ==. depositBankAccount] [] 
  case (convertToDense, mBankAccount) of
    (Just deposit, Just (Entity caId CustomerAccount {..})) -> do 
      let denseToUSD = USD deposit
      void . runDB $ update caId [CustomerAccountPrimaryBalance +=. denseToUSD]
      pure $ DepositResSuccess ("successfully deposited " ++ depositAmount ++ "!")
    (Nothing , Just _) -> pure DepositResParseFail
    (Just _, Nothing) -> pure DepositResCustomerNotFound
    (_,_) -> error "fail on both bankAccount and parse"

-- myTodo add this to isAuthenticated , also consider idempotency key so the same transfer can only go through once 
postWithdraw :: Handler WithdrawRes
postWithdraw = do
  WithdrawPost {..} <- requireCheckJsonBody
  mCustomerAccount <- runDB $ selectFirst [CustomerAccountBankAcount ==. withdrawPostBankAccount] []
  case mCustomerAccount of
    Just (Entity caId CustomerAccount {..}) -> do
      let withdrawalAmountDense :: Maybe (Dense "USD") = denseFromDecimal defaultDecimalConf withdrawPostAmount
      case withdrawalAmountDense of
        Just theDense -> do 
          if (customerAccountPrimaryBalance - (USD theDense)) < 0 then pure WithdrawResInsufficientFunds else do
            void $ runDB $ update caId [CustomerAccountPrimaryBalance -=. (USD theDense)]  
            pure $ WithdrawResSuccess withdrawPostAmount 
        Nothing -> pure WithdrawResParseAmountFail 


postTransfer :: Handler ()
postTransfer = do 
  -- TransferPost {..} <- requireCheckJsonBody
  -- afterWithdraw <- postWithdraw -- oh interesting, I thought about posting with a JSON to the handler 
  -- but no, this is good grounds for refactor out the Handler right now (think of Goose) because it's much 
  -- easier to call a pure function with the Data {..} rather than a post request. Handlers are tightly coupled now.
  pure ()


-- myTodo add this to isAuthenticated
postSupport :: Handler SupportRes
postSupport = do
  SupportPost {..} <- requireCheckJsonBody
  mCustomerAccount <- runDB $ selectFirst [CustomerAccountBankAcount ==. supportPostBankAccount] []  
  case mCustomerAccount of 
    Nothing -> pure $ SupportResNotFound
    Just (Entity caId ca) -> do
      now <- liftIO getCurrentTime  
      void . runDB . insert $ CustomerAccountSupport caId now supportPostText 
      pure $ SupportRes "we have received your request, we'll look into this and get back to you"



  -- return $ object
  --   [ "customerAccount" .= object
  --       [ "email" .= customerAccountEmail
  --       , "username" .= customerAccountUsername
  --       -- should token be renamed to something more specific like customertoken?
  --       , "token" .= token
  --       , "createdAt" .= customerAccountCreatedAt
  --       , "updatedAt" .= customerAccountUpdatedAt
  --       ]
  --   ]


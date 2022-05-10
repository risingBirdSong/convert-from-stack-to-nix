{-# LANGUAGE OverloadedStrings          #-}

module Validation.Email where

-- myTodo removed unused import
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

--------------------------------------------------------------------------------
-- Input validations ... note these are duplicated from Handler.User probably put them in Utils or something?

validEmail :: Monad m => Text -> ExceptT Text m Email
validEmail email =
  case mkEmail email of
     Just e -> return e
     _      -> throwError "Invalid email address."

uniqueEmail :: Email -> ExceptT Text Handler Email
uniqueEmail email = do
  user <- lift $ runDB $ getBy $ UniqueUserEmail email
  if isNothing user
    then return email
    else throwError "This email address is already being used."

uniqueUsername :: Text -> ExceptT Text Handler Text
uniqueUsername username = do
  user <- lift $ runDB $ getBy $ UniqueUserUsername username
  if isNothing user
    then return username
    else throwError "This username is already being used."

uniqueEmailIfChanged :: Email -> Email -> ExceptT Text Handler Email
uniqueEmailIfChanged currentEmail newEmail =
  if newEmail /= currentEmail
    then uniqueEmail newEmail
    else return newEmail

uniqueUsernameIfChanged :: Text -> Text -> ExceptT Text Handler Text
uniqueUsernameIfChanged currentUsername newUsername =
  if newUsername /= currentUsername
    then uniqueUsername newUsername
    else return newUsername
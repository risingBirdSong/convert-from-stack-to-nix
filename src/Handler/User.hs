{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Handler.User where

import           Control.Monad.Except      (ExceptT, throwError)
import           Data.Aeson                
import           Database.Persist.Extended
import           Import                    hiding (FormResult)
import           Web.Forma.Extended
import Money
--------------------------------------------------------------------------------
-- User login

type LoginFields = '[ "user", "aaa" , "email", "password" ]

data Login = Login
  { loginEmail    :: Email
  , loginPassword :: Text
  } deriving Show

loginForm :: Monad m => FormParser LoginFields Text m Login
loginForm =
  -- oh cool this is how the subparser works and it's strongly type according to LoginFields :: [Symbol]
  -- try making a typo
  -- so I didn't test this beyond the prexisting nesting of #user, but I read on docs
  -- subParser is a way to access nested jsons 
  -- https://hackage.haskell.org/package/forma-1.2.0/docs/Web-Forma.html
  subParser #user (subParser #aaa (Login
    <$> field #email (notEmpty >=> validEmail)
    <*> field #password notEmpty))

postUsersLoginR :: Handler Value
postUsersLoginR = do 
  liftIO $ print "--- postUsersLoginR"
  withForm loginForm $ \Login {..} -> do
    mUser <- runDB $ getBy $ UniqueUserEmail loginEmail
    case mUser of
      Just (Entity userId user@User {..}) -> do
        if (verifyPassword loginPassword userPassword) then encodeUser userId user else notAuthenticated
      _ ->
        notAuthenticated

getLoggedIn :: Handler ()
getLoggedIn = do 
  myheader <- lookupHeader "myheader"
  liftIO $ print "myheader"
  liftIO $ print myheader
  loggedin <-isAuthenticated
  liftIO $ print "loggedin"
  liftIO $ print loggedin
  pure ()

-- curl 'http://localhost:3000/api/articles/feed?limit=10&offset=0'   
--   -H 'Connection: keep-alive'   
--   -H 'sec-ch-ua: " Not A;Brand";v="99", "Chromium";v="99", "Google Chrome";v="99"'   
--   -H 'Accept: application/json, text/plain, */*'   
--   -H 'DNT: 1'   
--   -H 'Authorization: Token eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOjF9.G39DjYm3F3fotHvQ-sSGvelQFm7OyQt8EnL9YIhLmsI'   
--   -H 'sec-ch-ua-mobile: ?0'   
--   -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.51 Safari/537.36'   
--   -H 'sec-ch-ua-platform: "Linux"'   
--   -H 'Origin: http://localhost:3000'   
--   -H 'Sec-Fetch-Site: same-site'   
--   -H 'Sec-Fetch-Mode: cors'   
--   -H 'Sec-Fetch-Dest: empty'   
--   -H 'Referer: http://localhost:3000/'   
--   -H 'Accept-Language: en-US,en;q=0.9'   --compressed





--------------------------------------------------------------------------------
-- Register new user

type RegisterFields = '[ "user", "username", "email", "password" ]

data Register = Register
  { registerUsername :: Text
  , registerEmail    :: Email
  , registerPassword :: Text
  } deriving Show

registerForm :: FormParser RegisterFields Text Handler Register
registerForm =
  subParser #user (Register
    <$> field #username (notEmpty >=> uniqueUsername)
    <*> field #email (notEmpty >=> validEmail >=> uniqueEmail) -- oh thats cool, 
    <*> field #password notEmpty)

postUsersRegisterR :: Handler Value
postUsersRegisterR =
  withForm registerForm $ \Register {..} -> do
    pwdHash <- mkPassword registerPassword
    now <- liftIO getCurrentTime
    let user = User registerEmail registerUsername pwdHash "" defaultUserImage
                    now now
    userId <- runDB $ insert user
    users :: [Entity User] <- runDB $ selectList [] []
    liftIO $ print "users"
    liftIO $ print users
    encodeUser userId user

--------------------------------------------------------------------------------
-- Get current user

getUserR :: Handler Value
getUserR = 
  undefined
  -- do
  -- liftIO $ print "--- getUserR"
  -- lookup <- lookupSession credsKey
  -- liftIO $ print "lookup"
  -- liftIO $ print lookup
  -- mUserId <- maybeAuthId
  -- case mUserId of
  --   Nothing -> notAuthenticated
  --   Just userId -> do
  --     mUser <- runDB $ get userId
  --     case mUser of
  --       Nothing   -> notAuthenticated
  --       Just user -> encodeUser userId user

-- 
--------------------------------------------------------------------------------
-- Update current user

type UpdateFields = '[ "user", "username", "email", "password", "image", "bio" ]

data Update' = Update'
  { updateUsername :: Maybe Text
  , updateEmail    :: Maybe Email
  , updatePassword :: Maybe Text
  , updateImage    :: Maybe Text
  , updateBio      :: Maybe Text
  } deriving Show

updateForm :: User -> FormParser UpdateFields Text Handler Update'
updateForm User {..} =
  subParser #user (Update'
    <$> optional (field #username usernameValidation)
    <*> optional (field #email emailValidation)
    <*> optional (field #password notEmpty)
    <*> optional (field' #image)
    <*> optional (field' #bio))
  where
    usernameValidation = notEmpty >=> uniqueUsernameIfChanged userUsername
    emailValidation = notEmpty >=> validEmail >=> uniqueEmailIfChanged userEmail

putUserR :: Handler Value
putUserR = undefined
  -- do
  -- mUserId <- maybeAuthId
  -- case mUserId of
  --   Nothing -> notAuthenticated
  --   Just userId -> do
  --     mUser <- runDB $ get userId
  --     case mUser of
  --       Nothing -> notAuthenticated
  --       Just user ->
  --         withForm (updateForm user) $ \Update' {..} -> do
  --           now <- liftIO getCurrentTime
  --           pwdHash <-
  --             case updatePassword of
  --               Just pwd -> Just <$> mkPassword pwd
  --               _        -> return Nothing
  --           let updates =
  --                 catMaybes
  --                   [ maybeUpdate UserUsername updateUsername
  --                   , maybeUpdate UserEmail updateEmail
  --                   , maybeUpdate UserPassword pwdHash
  --                   , maybeUpdate UserImage updateImage
  --                   , maybeUpdate UserBio updateBio
  --                   , maybeUpdate UserUpdatedAt (Just now)
  --                   ]
  --           updatedUser <- runDB $ updateGet userId updates
  --           encodeUser userId updatedUser

--------------------------------------------------------------------------------
-- Input validations

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

--------------------------------------------------------------------------------
-- Helpers

defaultUserImage :: Text
defaultUserImage = "https://static.productionready.io/images/smiley-cyrus.jpg"

-- | Encode a 'User' with a JWT authentication token.
encodeUser :: UserId -> User -> Handler Value
encodeUser userId User {..} = do
  token <- userIdToToken userId
  return $ object
    [ "user" .= object
        [ "email" .= userEmail
        , "username" .= userUsername
        , "token" .= token
        , "bio" .= userBio
        , "image" .= userImage
        , "createdAt" .= userCreatedAt
        , "updatedAt" .= userUpdatedAt
        ]
    ]

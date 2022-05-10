{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foundation where

import qualified Auth.JWT             as JWT
import qualified Web.JWT              as JWT

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import qualified Yesod.Auth.Message   as AuthMsg


-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import           Data.Aeson           --(Result (Success), fromJSON)
import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Types.CustomerAccount
import Data.UUID
import Unsafe.Coerce
import Data.ByteString.Lazy.Internal
import Data.Aeson.Types

import           Data.Map             as Map (fromList, (!?))
import           Web.JWT              as JWT
import Data.Maybe


-- import Handler.CustomerAccount

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        fromMaybe (getApprootText guessApproot app req)
          (appRoot $ appSettings app)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    -- Routes requiring authentication.

    -- Routes not requiring authentication.
    -- my todo remove this catchall auth

    isAuthorized OneTimeCustomerAccountFundingRequest _ = isAuthenticated
    isAuthorized BalanceInquiry _ = isAuthenticated
    isAuthorized Deposit _ = isAuthenticated
    isAuthorized _ _ = return Authorized 
    isAuthorized CustomerAccountRegisterR _               = return Authorized
    isAuthorized UsersRegisterR _          = return Authorized


    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = CustomerAccountId
    -- i think this needs switched? or can there be multiple ones?
    -- type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = undefined -- myTodo change to an appropriate constructor, just wanted to type check for now
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = undefined -- myTodo change to an appropriate constructor, just wanted to type check for now
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authPlugins _ = []

    authHttpManager = error "Doesn't need an HTTP manager"

    authenticate _ =
      maybe (UserError AuthMsg.InvalidLogin) Authenticated <$> maybeAuthId

    maybeAuthId = do
      mToken <- JWT.lookupToken
      liftHandler $ maybe (return Nothing) tokenToUserId mToken


-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    liftIO $ print "muid"
    liftIO $ print muid
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

-- mytodo turn this into a maybeT ?
-- isEmailVerified :: Handler AuthResult
-- isEmailVerified = do 
--     liftIO $ print "hitting"
--     mBankAccount <- lookupHeader "bankaccount"
--     case mBankAccount of  
--         Just bankAccount -> do 
--             liftIO $ print "bankAccount"
--             liftIO $ print bankAccount
--             let convert = fromStrict bankAccount
--             liftIO $ print "convert"
--             liftIO $ print convert
--             let mBankAccountUuid = fromByteString (convert)
--             liftIO $ print "mBankAccountUuid"
--             liftIO $ print mBankAccountUuid

--             -- let mBankAccountUuid = fromByteString (unsafeCoerce bankAccount :: ByteString)
--             -- "bankAccount"
--             -- "f0240ec0-6f5b-4e08-9c67-6e53eec9c553"
--             -- "convert"
--             -- "f0240ec0-6f5b-4e08-9c67-6e53eec9c553"
--             -- "mBankAccountUuid"
--             -- Nothing

--             case mBankAccountUuid of
--                 Nothing -> pure $ Unauthorized "couldnt parse as a UUID"
--                 Just bankAccountUuid ->  do 
--                     mybCustomerAccount <- runDB $ getBy $ UniqueCustomerAccountBankAcount bankAccountUuid
--                     case mybCustomerAccount of 
--                         Nothing -> pure $ Unauthorized "bank account not found"
--                         Just (Entity caid ca) -> do 
--                             case customerAccountVerifiedStatus ca of 
--                                 UnverifiedEmail -> pure $ Unauthorized "please finish your registration by verifying your email" 
--                                 VerifiedEmail -> pure $ Authorized
--         Nothing -> pure $ Unauthorized "couldnt find bank account in the header"



data CustomerAccountUserVerify = CustomerAccountUserVerify {
    customerAccountUserVerifyId :: UUID
} deriving (Show, Generic)

instance ToJSON CustomerAccountUserVerify
instance FromJSON CustomerAccountUserVerify

data CustomerAccountUserVerifyTest = CustomerAccountUserVerifyTest {
    customerAccountUserVerifyTestId :: UUID , 
    testId :: UUID
} deriving (Show, Generic)

instance ToJSON CustomerAccountUserVerifyTest
instance FromJSON CustomerAccountUserVerifyTest

-- data ORJson = AA' CustomerAccountUserVerify | BB' CustomerAccountUserVerifyTest
--     deriving (Show, Generic)
-- instance ToJSON ORJson
-- instance FromJSON ORJson
-- "rca"
-- Error "parsing Foundation.ORJson failed, expected Object with key \"tag\" containing one of [\"AA'\",\"BB'\"], key \"tag\" not found"
-- 29/Mar/2022:22:41:53 -0700 [Error#yesod-core] src/Foundation.hs:(272,5)-(278,28): Non-exhaustive patterns in case
--  @(yesod-core-1.6.21.0-KnWkm1y5fItLdDbXAmFRE3:Yesod.Core.Class.Yesod src/Yesod/Core/Class/Yesod.hs:688:5)


-- foo :: (FromJSON a , FromJSON b) => a -> b 
-- foo from =  

isEmailVerifiedA :: Handler AuthResult
isEmailVerifiedA = do 
    rca <- parseCheckJsonBody
    case (rca :: Result CustomerAccountUserVerify) of
        Error str -> do 
            liftIO $ print "error" 
            liftIO $ print str 
        Success ca -> do 
            -- {
            --     "customerAccountUserVerifyId" : "2255eb10-8d92-4cbf-b189-d25b4f5a82b1"
            --     , "test" : "fdadf"
            -- }
            liftIO $ print "ca" 
            liftIO $ print ca
                
    -- Resul(ca@CustomerAccountUserVerify {..}) <- parseCheckJsonBody
    -- liftIO $ print "ca" 
    -- liftIO $ print ca 
    pure Authorized

-- isEmailVerifiedATest :: Handler AuthResult
-- isEmailVerifiedATest = do 
--     rca <- parseCheckJsonBody
--     liftIO $ print "rca"
--     liftIO $ print rca
-- --     "rca"
-- -- Error "parsing Foundation.ORJson failed, expected Object with key \"tag\" containing one of [\"AA'\",\"BB'\"], key \"tag\" not found"

--     case rca of
--         Success (AA' a) -> do 
--             liftIO $ print "a"
--             liftIO $ print a
--         Success (BB' b) -> do
--             liftIO $ print "b"
--             liftIO $ print b



    --    (Success CustomerAccountUserVerifyTest {..}) -> undefined 
    --    (Success CustomerAccountUserVerify {..}) -> undefined 
--         • Couldn't match expected type ‘CustomerAccountUserVerifyTest’
--                   with actual type ‘CustomerAccountUserVerify’
--     • In the pattern: CustomerAccountUserVerify {..}
--       In the pattern: Success CustomerAccountUserVerify {..}
--       In a case alternative:
--           (Success CustomerAccountUserVerify {..}) -> undefined
--     |
-- 263 |        (Success CustomerAccountUserVerify {..}) -> undefined 
 

    -- Resul(ca@CustomerAccountUserVerify {..}) <- parseCheckJsonBody
    -- liftIO $ print "ca" 
    -- liftIO $ print ca 
    
    pure Authorized





-- canMoveMoney :: Handler AuthResult
-- canMoveMoney = do 
--     isAuth <- isAuthenticated
--     isEmail <- isEmailVerified
--     if (all (== Authorized) [isAuth, isEmail]) then pure Authorized else pure $ Unauthorized "sorry we couldnt authorize you"
    -- pure Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding


userIdToToken :: UserId -> HandlerFor App Text
userIdToToken userId = do
  jwtSecret <- getJwtSecret
  return $ JWT.jsonToToken jwtSecret $ toJSON userId


customerAccountIdToToken :: CustomerAccountId -> HandlerFor App Text
customerAccountIdToToken customerAccountId = do
  jwtSecret <- getJwtSecret
  liftIO $ print "JWT.jsonToToken jwtSecret $ toJSON customerAccountId"
  liftIO $ print $ JWT.jsonToToken jwtSecret $ toJSON customerAccountId
  return $ JWT.jsonToToken jwtSecret $ toJSON customerAccountId

tokenToUserId :: Text -> Handler (Maybe CustomerAccountId)
tokenToUserId token = do
  liftIO $ print "token"
  liftIO $ print token
  jwtSecret <- getJwtSecret
  liftIO $ print "jwtSecret"
  liftIO $ print jwtSecret
  let mJwt = JWT.decodeAndVerifySignature (JWT.hmacSecret jwtSecret) token
  case mJwt of 
      Nothing -> pure Nothing  
      Just jwt -> do
        let unclaim = unClaimsMap (JWT.unregisteredClaims (JWT.claims (jwt))) !? "jwt"
--   let trytoverify = 
        liftIO $ print "unclaim"
        liftIO $ print unclaim
        let stepAAA = JWT.tokenToJson jwtSecret token
        liftIO $ print "stepAAA"
        liftIO $ print stepAAA
        let mCaId = fromJSON <$> JWT.tokenToJson jwtSecret token
--   liftIO $ print "mCaId"
--   liftIO $ print mCaId


-- eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJqd3QiOiI1NWNiZTNkMC03MmMzLTQ1ZGUtYjVlYy00NGU1YmMyYmM0NjEifQ.67cAZrPbJ6K_2-0sM-dd1uk39BGgHMlCO3o28ylzncM
-- Just (Verified (JOSEHeader {typ = Just "JWT", cty = Nothing, alg = Just HS256}) (JWTClaimsSet {iss = Nothing, sub = Nothing, aud = Nothing, exp = Nothing, nbf = Nothing, iat = Nothing, jti = Nothing, unregisteredClaims = ClaimsMap {unClaimsMap = fromList 
-- [("jwt",String "55cbe3d0-72c3-45de-b5ec-44e5bc2bc461")]}}) (Signature "67cAZrPbJ6K_2-0sM-dd1uk39BGgHMlCO3o28ylzncM"))
--   case stepAAA of
--     Just (String userId) -> do
--         liftIO $ print "success ----------------"
--         undefined 
--         -- return $ Just userId
--     _                     -> do

--         liftIO $ print "fail ----------------"
--         -- doreturn Nothing
--         undefined 
-- it was this:
        case mCaId of
            Just (Success userId) -> return $ Just userId
            _                     -> return Nothing

getJwtSecret :: HandlerFor App Text
getJwtSecret =
  getsYesod $ appJwtSecret . appSettings

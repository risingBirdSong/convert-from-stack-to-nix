{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE DataKinds #-}


module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import           Control.Monad.Logger                 (liftLoc, runLoggingT)

-- import           Handler.Articles
-- import           Handler.Profiles

import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)

import           Handler.User
import           Handler.CustomerAccount
import           Import
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       defaultShouldDisplayException,
                                                       getPort, runSettings,
                                                       setHost, setOnException,
                                                       setPort)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import qualified Prelude                              as P
import           System.Environment                   (lookupEnv)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)
import Money 
import Currency
import Data.Ratio
import Database.Persist.Sql
import Database.Persist.Types.Email.Internal
import Database.Persist.Types.Password.Internal
import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.UUID.V4 as UUID
import API.CustomerAccount
import Orphans
import DB.CustomerAccount
import DB.ImportEsqueleto
import Types.CustomerAccount


 



-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    
    liftIO $ print "pool"
    liftIO $ print pool

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc $ flip runSqlPool pool $ runMigration migrateAll

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  settings <- getAppSettings
  return $
    logWare $
    defaultMiddlewaresNoLogging $
    (corsified $ appCorsOriginWhitelist settings) appPlain

-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: [Text] -> Middleware
corsified = cors . const . Just . appCorsResourcePolicy

-- | CORS resource policy to be used with 'corsified' middleware.
appCorsResourcePolicy :: [Text] -> CorsResourcePolicy
appCorsResourcePolicy corsWhitelist = CorsResourcePolicy {
    corsOrigins        = Just (encodeUtf8 <$> corsWhitelist, False)
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
}

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = do
  checkEnvironment
  loadYamlSettings [configSettingsYml] [] useEnv

checkEnvironment :: IO ()
checkEnvironment = do
  mJwtSecret <- lookupEnv "JWT_SECRET"
  case mJwtSecret of
    Nothing ->
      P.errorWithoutStackTrace "Set the \"JWT_SECRET\" environment variable"
    _ -> return ()

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB

-- wat. my is this is this query finding users. but psql is not... ??

allusers :: Handler () 
allusers = do 
  users :: [Entity User] <- runDB $ selectList [] []
  liftIO $ print "users" 
  liftIO $ print users 
  pure ()

storeSomeData :: Handler ()
storeSomeData = do 
  void $ runDB . insert $ SomeData "testing" (10%1) (TestUSD (2 :: Dense "USD"))
  pure ()

-- in psql
-- bankService=# select * from some_data;
--  id | something |            balance            
-- ----+-----------+-------------------------------
--   1 | testing   | USD {unUSD = Dense "USD" 2%1}

getSomeData :: Handler ()
getSomeData = do 
  somedatas :: ([Entity SomeData]) <- runDB $ selectList [] []
  liftIO $ print "somedatas"
  liftIO $ print somedatas
  pure ()

plusplusmoneyhandler :: Handler ()
plusplusmoneyhandler = do 
  -- sid <- runDB $ insert $ SomeData "aaa" (5%1) (USD (2 :: Dense "USD"))
  -- liftIO $ print "sid" 
  -- liftIO $ print sid 
  -- let unkeyed = unSomeDataKey sid
  let from = fromBackendKey (1 ) :: Key SomeData
  void $ runDB $  updatemoneytest from 
  pure ()

insertCustomer :: Handler ()
insertCustomer = do
  now <- liftIO $ getCurrentTime  
  let email = fromJust $ mkEmail "aaa@aaa.com"
  pass <- mkPassword "keepbasic"
  -- let mybBalance = dense (341 % 100) :: Maybe (Dense "USD")
  let mybBalance = ( USD (2 :: Dense "USD"))
  bankAccount <- liftIO $ UUID.nextRandom
  customerid <- runDB . insert $ CustomerAccount bankAccount ("myUser" :: Text) pass now now (mybBalance)
  pure ()


-- steps A) B) and C) below

-- A)
-- PSQL
-- bankService=# select * from customer_account;
--  id | email | username | password | created_at | updated_at | balance 
-- ----+-------+----------+----------+------------+------------+---------
-- (0 rows)

-- B)
-- REPL
-- Ok, 15 modules loaded.
-- *Application> handler insertCustomer

-- C)
-- PSQL
-- bankService=# select * from customer_account;
--  id |    email    | username |                                    password                                     |          created_at           |          updated_at           |        balance         
-- ----+-------------+----------+---------------------------------------------------------------------------------+-------------------------------+-------------------------------+------------------------
--   1 | aaa@aaa.com | aaa      | sha256|14|YA/mFbNw5ZrDfNeV0L4/0g==|zEtDACTqa6PXoh1KuBMrufU/M2UGE5IhUUYCCl4UrwA= | 2022-03-14 07:53:00.997017-07 | 2022-03-14 07:53:00.997017-07 | USD' (Dense "USD" 2%1)
-- (1 row)




{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}



module Model.LoginUser where

-- import Control.Monad.Except (ExceptT, throwError)
-- import Data.Aeson -- why isnt this module recognized
-- import           Database.Persist.Extended
-- import           Import                    hiding (FormResult)
-- import           Web.Forma
-- import           Web.Forma.Extended
-- import Money

-- import           Blaze.ByteString.Builder           
-- import           Blaze.ByteString.Builder.Char.Utf8 
-- import           Control.Concurrent                 
-- -- import           Control.Monad                     
-- import           Data.Monoid                        
-- import           Network.Wai                        
-- import           Yesod.Core 
-- import           Validation.Email
-- import           Database.Persist.Types.Email.Internal
-- import           Data.Maybe
-- import qualified Data.UUID.V4 as UUID
-- import Types.CustomerAccount
-- import Currency
-- import Data.Ratio
-- import Database.Persist.Types.Password.Internal



-- type LoginFields = '["username" , "password"]
-- data Login = Login
-- -- the order matters and aligns with the order in the applicatives in LoginForm
--   { loginUsername :: Text,
--     loginPassword :: Text
--   }
--   deriving (Show)

-- loginForm :: FormParser LoginFields Text Handler Login
-- loginForm = Login
--     <$> field #username (notEmpty)
--     <*> field #password notEmpty -- so this is a shaw256 now.. verify this is a password
--     -- https://economic-ecosystem.slack.com/archives/C036E6P9GUX/p1648869507972559

-- -- avoid boolean blindness 
-- -- data LoginResults = LoginSuccess | LoginFail
-- --     deriving (Show, Eq, Generic)

-- -- -- why does this type sig need rankNTypes {-# LANGUAGE RankNTypes #-}
-- -- loginLogic :: Login -> DB (Maybe (Entity CustomerAccount))
-- -- loginLogic Login {..} = selectFirst [CustomerAccountPrimaryUsername ==. loginUsername] []
        

-- wha?? why does stuff like #password line 47 work in registerUser.hs but not here?
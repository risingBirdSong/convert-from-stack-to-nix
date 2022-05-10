{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Database.Persist.Types.Password.Internal
  ( Password(..)
  , mkPassword
  , verifyPassword
  ) where

import           ClassyPrelude.Yesod
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Database.Persist.Sql (PersistFieldSql (..))
import qualified Yesod.Auth.Util.PasswordStore as PS
import Data.Aeson


-- | Password is stored as a hash.
newtype Password = Password
  { unPassword :: Text }
  deriving (Show, Eq, Generic)

-- | Instantiate a 'Password' from 'Text'.
mkPassword :: MonadIO m => Text -> m Password
mkPassword text =
  Password . decodeUtf8 <$> liftIO (PS.makePassword (encodeUtf8 text) 14)

mkPasses :: IO ()
mkPasses = do 
  psa <- mkPassword "abc"
  psb <- mkPassword "bcd123"
  print psa 
-- psa = mkPassword ""


-- myTodo make a verifyThatItIsAPassword func that just makes sure it's a SHA256 and is the proper length

-- | Check a raw password against DB password.
verifyPassword :: Text -> Password -> Bool
verifyPassword rawPassword Password {..} =
  PS.verifyPassword (encodeUtf8 rawPassword) $ encodeUtf8 unPassword

instance PersistField Password where
  toPersistValue Password {..} = PersistText unPassword
  fromPersistValue (PersistText text) = Right $ Password text
  fromPersistValue x =
    Left $
    modulePath <>
    "When trying to deserialize Password: expected PersistText, received: " <>
    T.pack (show x)

instance PersistFieldSql Password where
  sqlType _ = SqlString

instance ToJSON Password where
  toJSON Password {..} = String unPassword

instance FromJSON Password

modulePath :: Text
modulePath = "Database/Persist/Types/Password/Internal.hs: "

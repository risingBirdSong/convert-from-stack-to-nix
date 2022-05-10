{-# LANGUAGE TemplateHaskell            #-}

module Types.CustomerAccount where 

import           ClassyPrelude.Yesod
import           Database.Persist


data CustomerAccountVerifiedStatus = UnverifiedEmail | VerifiedEmail
    deriving (Eq, Show, Read, Ord)

-- https://stackoverflow.com/questions/46019916/no-instance-error-with-yesod-persistent-and-mysql
derivePersistField "CustomerAccountVerifiedStatus"
<!-- make a bank account model, should it have two different models for personal and bank ? -->
[x] change the ids to UUIDs in config/models.persistentmodels

PersonalAccount
    email Email
    username Text
    password Password
    createdAt UTCTime
    updatedAt UTCTime
    UniqueUserUsername username
    UniqueUserEmail email
    deriving Show Typeable
    <!-- balance  -->

[x] 
    change from 
        Handler Value 
    to 
        Handler SpecificType



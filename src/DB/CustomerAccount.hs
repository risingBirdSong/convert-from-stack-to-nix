{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module DB.CustomerAccount where 

-- import Database.Esqueleto.Experimental
import Model
import DB.ImportEsqueleto
import Database.Esqueleto (SqlExpr)
import Data.Ratio


-- givemoney :: DB ()
-- givemoney =
--     -- customers <- select $ from $ table @CustomerAccount 
--     -- mytodo update pure return 
--     --QQQ hmm with the type hole 'relevant bindings include' doesnt have the variables available from the {..} is there a way to make these available? 
--     -- let newcustomers = map (\ cst@(Entity {..}) -> Entity cst {customerAccountPrimaryBalance = ((1+) <$> customerAccountPrimaryBalance) }) customers
--     update $ \sqlca -> do
                -- ahhh, SqlExpr is a type not a data constructor so i cant pattern match on it 
--                 let match@(SqlExpr ca) = sqlca 
--                 -- let oldValue = customerAccountPrimaryBalance ca 
--                 -- oldValue :: ()
--                 undefined

        -- ah so how to make an instance for currency?? maybe good session for Goose or #learnHaskell
--   • No instance for (Num Util.Util.Currency)
--         arising from the literal ‘1’
--     • In the first argument of ‘val’, namely ‘1’
--       In the first argument of ‘just’, namely ‘(val 1)’
--       In the second argument of ‘(+=.)’, namely ‘just (val 1)’
--    |

                                                            
--         -- set ca [ CustomerAccountPrimaryBalance =. (val ( oldValue)) ]
--     -- pure newcustomers
--     undefined

-- givemoneytest :: DB [SomeData]



updatemoneytest :: Key SomeData -> DB ()
updatemoneytest sid = update $ \test -> do 
        set test [SomeDataSomeNum +=. (val (10%1))]
        where_ $ (test ^. SomeDataId) ==. (val sid)
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Orphans where

import           ClassyPrelude.Yesod
import Data.Aeson
import Yesod (ToContent (..), ToTypedContent (..), TypedContent (..), typeJson)

instance {-# OVERLAPPABLE #-} ToJSON a => ToTypedContent a where
  toTypedContent = TypedContent typeJson . toContent

instance {-# OVERLAPPABLE #-} ToJSON a => ToContent a where
  toContent = toContent . encode


-- oh yeah, so we don't have to call JSONResponse in our Handler, nice
-- https://hackage.haskell.org/package/yesod-core-1.6.21.0/docs/Yesod-Core-Types.html#t:JSONResponse
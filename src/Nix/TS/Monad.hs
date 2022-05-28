{-# language GeneralizedNewtypeDeriving #-}
module Nix.TS.Monad
( TypeM
)
where

import Nix.TS.Types
import Nix.Scope


newtype TypeM a = TypeM (ReaderT (Scopes TypeM NTypeLoc) (Either Text) a)
  deriving (Functor, Applicative, Monad, MonadReader (Scopes TypeM NTypeLoc))

instance Scoped NTypeLoc TypeM where
  askScopes = askScopesReader
  clearScopes = clearScopesReader @TypeM @NTypeLoc
  pushScopes = pushScopesReader
  lookupVar = lookupVarReader

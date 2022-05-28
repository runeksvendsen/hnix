{-# language GeneralizedNewtypeDeriving #-}
module Nix.TS.Monad
( TypeM
, runTypeM
, retEither
)
where

import Nix.TS.Types
import Nix.Scope


runTypeM :: TypeM a -> Either Text a
runTypeM = (`runReaderT` mempty) . unTypeM

newtype TypeM a = TypeM {unTypeM :: ReaderT (Scopes TypeM NTypeLoc) (Either Text) a}
  deriving (Functor, Applicative, Monad, MonadReader (Scopes TypeM NTypeLoc))

retEither :: Either Text a -> TypeM a
retEither = TypeM . lift

instance Scoped NTypeLoc TypeM where
  askScopes = askScopesReader
  clearScopes = clearScopesReader @TypeM @NTypeLoc
  pushScopes = pushScopesReader
  lookupVar = lookupVarReader

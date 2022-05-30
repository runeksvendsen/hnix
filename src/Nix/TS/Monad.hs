{-# language GeneralizedNewtypeDeriving #-}
-- {-# options_ghc -Wno-missing-methods #-}
module Nix.TS.Monad
( TypeM
, runTypeM
, retEither
, prettyTypeM
)
where

import Nix.TS.Types
import Nix.Scope
import Nix.Eval (MonadEval)
import Nix.Value.Monad (MonadValue(..))
import Nix.Convert (ToValue, FromValue)
import Nix (NixString, AttrSet, PositionSet)
import Control.Monad.Fix (MonadFix)


runTypeM :: TypeM a -> Either Text a
runTypeM = (`runReaderT` mempty) . unTypeM

newtype TypeM a = TypeM {unTypeM :: ReaderT (Scopes TypeM NTypeLoc) (Either Text) a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadReader (Scopes TypeM NTypeLoc))

prettyTypeM :: Show a => TypeM a -> Text
prettyTypeM = either ("Error: " <>) show . runTypeM

retEither :: Either Text a -> TypeM a
retEither = TypeM . lift

instance Scoped NTypeLoc TypeM where
  askScopes = askScopesReader
  clearScopes = clearScopesReader @TypeM @NTypeLoc
  pushScopes = pushScopesReader
  lookupVar = lookupVarReader

instance MonadEval NTypeLoc TypeM
instance MonadValue NTypeLoc TypeM where
  defer = id
  demand = pure
  inform = pure
instance ToValue Bool TypeM NTypeLoc
instance ToValue [NTypeLoc] TypeM NTypeLoc
instance FromValue NixString TypeM NTypeLoc
instance ToValue (AttrSet NTypeLoc, PositionSet) TypeM NTypeLoc
instance FromValue (AttrSet NTypeLoc, PositionSet) TypeM NTypeLoc

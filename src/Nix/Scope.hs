{-# LANGUAGE UndecidableInstances #-}
{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Nix.Scope
( Scope, mkScope, unScope
, Scopes(lexicalScopes, dynamicScopes)
, scopeLookup
, Scoped(..)
, askScopesReader, clearScopesReader, pushScope, pushWeakScope, pushScopesReader, lookupVarReader, withScopes)
 where

import qualified Data.HashMap.Lazy             as M
import qualified Data.HashMap.Internal         as HMI
import qualified Text.Show
import           Lens.Family2
import           Nix.Expr.Types
import qualified Debug.Trace

showHM :: HashMap k v -> String
showHM hm =
  case hm of
    HMI.Empty -> "Empty"
    HMI.BitmapIndexed _ _ -> "BitmapIndexed bitmap array"
    HMI.Leaf _ _ -> "Leaf hash leaf"
    HMI.Full _ -> "Full array"
    HMI.Collision _ _ -> "Collision hash array"

--  2021-07-19: NOTE: Scopes can gain from sequentiality, HashMap (aka AttrSet) may not be proper to it.
newtype Scope a = Scope (AttrSet a)
  deriving
    ( Eq, Ord, Generic
    , Typeable, NFData
    , Read, Hashable
    , Semigroup, Monoid
    , Functor, Foldable, Traversable
    , One
    )

mkScope :: AttrSet a -> Scope a
mkScope = Scope

unScope :: Scope a -> AttrSet a
unScope (Scope a) = a

instance Show (Scope a) where
  show (Scope m) = "Scope: " <> showHM m

scopeLookup :: VarName -> [Scope a] -> Maybe a
scopeLookup key = foldr fun Nothing
 where
  fun
    :: Scope a
    -> Maybe a
    -> Maybe a
  fun (Scope m) rest = M.lookup key m <|> rest

data Scopes m a =
  Scopes
    { lexicalScopes :: ![Scope a]
    , dynamicScopes :: [m (Scope a)]
    }

instance Show (Scopes m a) where
  show (Scopes m a) =
    "Scopes: " <> show m <> ", and " <> show (length a) <> " with-scopes"

instance Semigroup (Scopes m a) where
  Scopes ls lw <> Scopes rs rw = Scopes (ls <> rs) (lw <> rw)

instance Monoid (Scopes m a) where
  mempty = emptyScopes

emptyScopes :: Scopes m a
emptyScopes = Scopes mempty mempty

class Scoped a m | m -> a where
  askScopes :: m (Scopes m a)
  clearScopes   :: m r -> m r
  pushScopes    :: Scopes m a -> m r -> m r
  lookupVar     :: VarName -> m (Maybe a)

askScopesReader
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => m (Scopes m a)
askScopesReader = askLocal

clearScopesReader
  :: forall m a e r
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => m r
  -> m r
clearScopesReader = local $ set hasLens $ emptyScopes @m @a

pushScope
  :: Scoped a m
  => Scope a
  -> m r
  -> m r
pushScope scope = do
  Debug.Trace.traceM ("pushScope: " <> show scope)
  pushScopes $ Scopes (one scope) mempty

pushWeakScope
  :: ( Functor m
     , Scoped a m
     )
  => m (Scope a)
  -> m r
  -> m r
pushWeakScope scope = pushScopes $ Scopes mempty $ one scope

pushScopesReader
  :: ( MonadReader e m
     , Has e (Scopes m a)
     )
  => Scopes m a
  -> m r
  -> m r
pushScopesReader s = local $ over hasLens (s <>)

lookupVarReader
  :: forall m a e
  . ( MonadReader e m
    , Has e (Scopes m a)
    )
  => VarName
  -> m (Maybe a)
lookupVarReader k =
  do
    Debug.Trace.traceM $ "lookupVarReader: " <> show k
    let lexicalScopes' = lexicalScopes @m . view hasLens
    lexicalScopesAsk <- asks lexicalScopes'
    Debug.Trace.traceM $ "lexicalScopes length: " <> show (length lexicalScopesAsk)
    Debug.Trace.traceM $ "lexicalScopes head: " <> show (viaNonEmpty Prelude.head lexicalScopesAsk)
    Debug.Trace.traceM $ "lexicalScopes: " <> show (lexicalScopesAsk :: [Scope a])

    mres <- asks $ scopeLookup k . lexicalScopes'
    Debug.Trace.traceM $ "mres: " <> maybe "mres Nothing" (const "mres Just") mres

    maybe
      (do
        ws <- asks $ dynamicScopes . view hasLens

        foldr
          (\ weakscope rest ->
            do
              mres' <- M.lookup k . coerce @(Scope a) <$> weakscope

              maybe
                rest
                (pure . pure)
                mres'
          )
          (pure Nothing)
          ws
      )
      (pure . pure)
      mres

withScopes
  :: Scoped a m
  => Scopes m a
  -> m r
  -> m r
withScopes scopes = clearScopes . pushScopes scopes

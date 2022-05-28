{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-orphans #-}

module Nix.TS.Types
( TAtom(..)
, NTypeF(..)
, TypeAnn
, NTypeLoc
, NType
)
where

import Nix.Expr.Types
import Nix.Expr.Types.Annotated (NExprLoc, AnnF, Ann, SrcSpan)
import Data.Fix (Fix)
-- TH
import           Text.Show.Deriving             ( deriveShow1 )
import           Data.Eq.Deriving               ( deriveEq1     )
import           Data.Ord.Deriving              ( deriveOrd1   )

data TAtom
  = TURI
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TPath
  deriving
    ( Eq
    , Ord
    , Generic
    , Typeable
    , Show
    , Read
    )

data NTypeF r
  = TConstant !TAtom
  | TList ![r]
  | TSet Recursivity !(AttrSet (NTypeF r))
  | TFun !r !r
  -- ^ A single-argument function.
  --   Types: (1) argument type (2) return type.
  | TIf !NExprLoc !r !r
  -- ^ If-then-else statement.
  --
  -- > TIf x y z                                   ~  if x then y else z
  deriving
    ( Eq, Ord, Generic
    , Typeable
    , Functor, Foldable, Traversable
    , Show
    )

$(deriveShow1 ''NTypeF)
$(deriveEq1   ''NTypeF)
$(deriveOrd1  ''NTypeF)

type TypeAnn = Ann SrcSpan NTypeF

type NTypeLoc = Fix (AnnF SrcSpan NTypeF)
type NType = Fix NTypeF

{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# options_ghc -Wno-orphans #-}

module Nix.TS.Types where

import Nix.Expr.Types
import Nix.Expr.Types.Annotated (NExprLoc, AnnF, SrcSpan)
import Data.Fix (Fix(..))
import Nix.Atoms (NAtom (..))
-- TH
import           Text.Show.Deriving             ( deriveShow1 )
import           Data.Eq.Deriving               ( deriveEq1     )
import           Data.Ord.Deriving              ( deriveOrd1   )
import Text.Printf (printf)
import qualified Data.Text as T
import Nix.Eval (MonadEval(..))
import Nix.Value.Monad (MonadValue)
import Nix.Convert (ToValue, FromValue)
import Nix.String (NixString)

number :: [NTypeF r]
number = [TConstant TInt, TConstant TFloat]

inferAtom :: NAtom -> TAtom
inferAtom = \case
  NURI _ -> TURI
  NInt _ -> TInt
  NFloat _ -> TFloat
  NBool _ -> TBool
  NNull -> TNull

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
  | TSet Bool !(AttrSet (NTypeF r))
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

type Variadic = Bool

$(deriveShow1 ''NTypeF)
$(deriveEq1   ''NTypeF)
$(deriveOrd1  ''NTypeF)

ret :: f (Fix f) -> Either e (Fix f)
ret = pure . mk

mk :: f (Fix f) -> Fix f
mk = Fix

instance MonadEval NType (Either Text) where
  freeVariable a = Left $ "freeVariable: " <> show a
  synHole a = Left $ "synHole: " <> show a
  attrMissing a b = Left $ "attrMissing: " <> show (a, b)
  evaledSym _ = pure
  evalCurPos = Left "evalCurPos"

  evalConstant na = ret $ TConstant (inferAtom na)
  evalString = const $ ret $ TConstant TStr
  evalLiteralPath = const $ ret $ TConstant TPath
  evalEnvPath = const $ ret $ TConstant TPath
  evalUnary nuo ty =
    case nuo of
      NNeg ->
        if ty `elem` fmap mk number
          then ret $ TFun ty ty
          else err nuo "negate" ty
      NNot ->
        if ty == mk (TConstant TBool)
          then ret $ TFun (mk $ TConstant TBool) (mk $ TConstant TBool)
          else err nuo "apply 'logical or' to" ty
  evalBinary nbo r1 r2m = do
    r2 <- r2m
    {- TODO: assert -}
    let bool' = TConstant TBool
        num = TConstant TInt
    case nbo of
      -- Logic
      NEq -> ret bool'
      NNEq -> ret bool'
      NLt -> ret bool'
      NLte -> ret bool'
      NGt -> ret bool'
      NGte -> ret bool'
      NAnd -> ret bool'
      NOr -> ret bool'
      NImpl -> ret bool'
      -- Set union
      NUpdate -> ret $ TSet False (error "TODO")
      -- Arithmetic
      NPlus -> ret num
      NMinus -> ret num
      NMult -> ret num
      NDiv -> ret num
      -- List concat
      NConcat ->
        let getList (TList l) = Right l
            getList badType = err nbo "concatenate" badType
        in do
          l1 <- getList (unFix r1)
          l2 <- getList (unFix r2)
          ret $ TList (l1 <> l2)
      NApp ->
        let handleFun (TFun funArg ret') arg
              | funArg == arg = Right ret'
              | otherwise = Left $ T.pack $ printf "Function argument has type %s but actual argument has type %s" (show funArg :: T.Text) (show arg :: T.Text)
            handleFun badType arg = err nbo (printf "apply to %s" (show arg :: T.Text)) badType
        in handleFun (unFix r1) r2
  evalWith = error "TODO"
  evalIf = error "TODO"
  evalAssert = error "TODO"
  evalApp = error "TODO"
  evalAbs _ _ = error "TODO"
  evalError = error "TODO"

err :: (Show expr, Show ty) => expr -> String -> ty -> Either Text a
err expr str badType =
  Left $ unlines
    [ toText @String $ printf "Cannot %s value of type %s" (str :: String) (show badType :: T.Text)
    , "Expression: " <> show expr
    ]

type NTypeLoc = Fix (AnnF SrcSpan NTypeF)
type NType = Fix NTypeF

-- instance Scoped NType (Either Text) where
--   askScopes   = askScopesReader
--   clearScopes = clearScopesReader @(Either Text) @NExprLoc
--   pushScopes  = pushScopesReader
--   lookupVar   = lookupVarReader

instance MonadValue NType (Either Text) where
instance ToValue (AttrSet NType, PositionSet) (Either Text) NType
instance ToValue [NType] (Either Text) NType
instance ToValue Bool (Either Text) NType
instance FromValue (AttrSet NType, PositionSet) (Either Text) NType
instance FromValue NixString (Either Text) NType

-- check :: NExpr -> Either Text NType
-- check = foldFix Eval.evalTrace

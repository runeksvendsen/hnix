{-# language TemplateHaskell #-}

module Nix.TS.Types where

import Nix.Expr.Types
import Nix.Expr.Types.Annotated (NExprLoc, AnnF)
import Data.Fix (Fix(..))
import Nix.Atoms (NAtom (..))
import Control.Arrow ((>>>))
-- TH
import           Text.Show.Deriving             ( deriveShow1, deriveShow2 )
import           Text.Read.Deriving             ( deriveRead1, deriveRead2 )
import           Data.Eq.Deriving               ( deriveEq1  , deriveEq2   )
import           Data.Ord.Deriving              ( deriveOrd1 , deriveOrd2  )
import Text.Printf (printf)
import qualified Data.Text as T

infer :: Fix NExprF -> Fix NTypeF
infer =
  Fix . inferExpr . unFix
  where
    mk = Fix
    inferExpr expr =
      let
        err str badType =
          error $ unlines [toText @String $ printf "Cannot %s value of type %s" (str :: String) (show badType :: T.Text), show expr]
      in case expr of
      NConstant na -> TConstant (inferAtom na)
      NStr _ -> TConstant TStr
      NSym vn -> _lookupVariableInEnv
      NList rs -> TList $ fmap infer rs
      NSet re binds -> _
      NLiteralPath _ -> TConstant TPath
      NEnvPath _ -> TConstant TPath
      NUnary nuo r ->
        let ty = infer r
        in case nuo of
          NNeg ->
            if ty `elem` fmap mk number
              then TFun ty ty
              else err "negate" ty
          NNot ->
            if ty == mk (TConstant TBool)
              then TFun (mk $ TConstant TBool) (mk $ TConstant TBool)
              else err "apply 'logical or' to" ty
      NBinary nbo r r' ->
        {- TODO: assert -}
        let bool' = TConstant TBool
            num = TConstant TInt
        in case nbo of
          -- Logic
          NEq -> bool'
          NNEq -> bool'
          NLt -> bool'
          NLte -> bool'
          NGt -> bool'
          NGte -> bool'
          NAnd -> bool'
          NOr -> bool'
          NImpl -> bool'
          -- Set union
          NUpdate -> TSet False (error "TODO")
          -- Arithmetic
          NPlus -> num
          NMinus -> num
          NMult -> num
          NDiv -> num
          -- List concat
          NConcat -> TList (fmap infer r <> fmap infer r')
          NApp -> _
      NSelect m_r r ne -> _
      NHasAttr r ne -> _
      NAbs pa r -> _
      NLet binds r -> _
      NIf r r' r3 -> _
      NWith r r' -> _
      NAssert r r' -> _
      NSynHole vn -> _

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

type Variadic = Bool

$(deriveShow1 ''NTypeF)
$(deriveEq1   ''NTypeF)
$(deriveOrd1  ''NTypeF)

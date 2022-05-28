{-# language PatternSynonyms #-}
{-# options_ghc -Wno-orphans #-}

module Nix.TS.Infer
( infer
, NTypeLoc
, NType
)
where

import Nix.TS.Types

import Nix.Expr.Types
import Nix.Expr.Types.Annotated (NExprLoc, AnnF, SrcSpan, pattern AnnF, pattern Ann)
import Data.Fix (foldFix)
import Nix.Atoms (NAtom (..))
import Text.Printf (printf)
import qualified Data.Text as T
-- TH


number :: [NTypeF r]
number = [TConstant TInt, TConstant TFloat]

inferAtom :: NAtom -> TAtom
inferAtom = \case
  NURI _ -> TURI
  NInt _ -> TInt
  NFloat _ -> TFloat
  NBool _ -> TBool
  NNull -> TNull

infer :: NExprLoc -> Either Text NTypeLoc
infer = foldFix infer'

infer'
  :: AnnF SrcSpan NExprF (Either Text NTypeLoc)
  -> Either Text NTypeLoc
infer' (AnnF src expr) =
  let ret = pure . Ann src
  in case expr of
    NConstant na -> ret $ TConstant (inferAtom na)
    NStr _ -> ret $ TConstant TStr
    NLiteralPath _ -> ret $ TConstant TPath
    NEnvPath _ -> ret $ TConstant TPath
    NSym _ -> error "TODO: lookup"
    NList itemsE -> do
      items <- sequenceA itemsE
      ret $ TList items
    NSet rec' bindings -> error "TODO"
    NUnary nuo t1m ->
      t1m >>= evalUnary ret nuo
    NBinary op t1m t2m -> do
      t1 <- t1m
      t2 <- t2m
      evalBinary ret op t1 t2
    NSelect _ _ _ -> ret $ TSet NonRecursive mempty {- TODO -}
    NHasAttr _ _ -> ret $ TConstant TBool
    NAbs argPE retE -> do
      ret' <- retE
      let f :: [(VarName, Maybe (Either Text r))] -> Either Text [(VarName, Maybe r)]
          f = traverse (traverse sequenceA)
      arg <- case argPE of
        (Param varName) -> pure $ Param varName
        ParamSet mVarName variadic paramSetE -> do
          paramSet <- f paramSetE
          pure $ ParamSet mVarName variadic paramSet
      arg' <- evalParams arg
      ret $ TFun arg' ret'
    NLet lets' in' -> undefined
    NIf cond then' else' -> undefined
    NWith attrs scope -> undefined
    NAssert _ _ -> undefined
    NSynHole _ -> undefined

evalParams :: Params NTypeLoc -> Either Text NTypeLoc
evalParams = error "TODO"

evalUnary
  :: (NTypeF TypeAnn -> Either Text TypeAnn)
  -> NUnaryOp
  -> TypeAnn
  -> Either Text TypeAnn
evalUnary ret nuo (Ann src ty) =
  let mk = Ann src
  in case nuo of
    NNeg ->
      if ty `elem` number
        then ret $ TFun (mk ty) (mk ty)
        else err nuo "negate" ty
    NNot ->
      if ty == TConstant TBool
        then ret $ TFun (mk $ TConstant TBool) (mk $ TConstant TBool)
        else err nuo "apply 'logical or' to" ty

evalBinary
  :: (NTypeF TypeAnn -> Either Text TypeAnn)
  -> NBinaryOp
  -> NTypeLoc
  -> NTypeLoc
  -> Either Text TypeAnn
evalBinary ret nbo (Ann _ r1) (Ann _ r2) = do
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
    NUpdate -> ret $ TSet NonRecursive mempty {- TODO -}
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
        l1 <- getList r1
        l2 <- getList r2
        ret $ TList (l1 <> l2)
    NApp ->
      let handleFun (TFun (Ann _ funArg) ret') arg
            | funArg == arg = Right ret'
            | otherwise = Left $ T.pack $ printf "Function argument has type %s but actual argument has type %s" (show funArg :: T.Text) (show arg :: T.Text)
          handleFun badType arg = err nbo (printf "apply to %s" (show arg :: T.Text)) badType
      in handleFun r1 r2

err :: (Show expr, Show ty) => expr -> String -> ty -> Either Text a
err expr str badType =
  Left $ unlines
    [ toText @String $ printf "Cannot %s value of type %s" (str :: String) (show badType :: T.Text)
    , "Expression: " <> show expr
    ]

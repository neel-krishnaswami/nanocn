# Plan: Merge Implementation to Match Unified Ott Spec

(This plan was provided by the user and guides the implementation.)

## Overview

Unify computation and assertion languages into a single expression language
with a three-level effect system (pure | impure | spec). Merge duplicate
modules (two expression types, two typecheckers, two elaborators, two contexts)
into single unified versions.

## Key Changes

- Effect: Pure | Impure | Spec with purify
- Context: unified extend x sort eff
- Signature: FunSig only (SpecFun/SpecVal removed)
- CoreExpr: merged from Expr + CoreExpr (18 variants)
- SurfExpr: merged from SurfComp + SurfExpr
- One Elaborate, one Typecheck, one EvalCtx, one CtorLookup
- Parser: merged expr/spec_expr grammars
- Function declarations: fun f : sort -> sort [eff] = { branches }

## Implementation Order

Bottom-up: Effect -> Sort -> CoreExpr -> SurfExpr -> EvalCtx -> Context ->
Sig -> CtorLookup -> Prog -> Prim -> Elaborate -> Typecheck -> Parser ->
Main -> Cleanup -> Tests + Examples

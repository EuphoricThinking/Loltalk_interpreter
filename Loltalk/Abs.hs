-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language loltalk.

module Loltalk.Abs where

import Prelude (String)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable
  , Int, Maybe(..)
  )
import qualified Data.String

type Type = Type' BNFC'Position
data Type' a = Int a | Str a | Bool a | Void a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Program = Program' BNFC'Position
data Program' a = Program a [TopDef' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type TopDef = TopDef' BNFC'Position
data TopDef' a
    = FnDef a (Type' a) MIdent [Arg' a] [Stmt' a]
    | FnDefRet a (Type' a) MIdent [Arg' a] [Stmt' a]
    | DeclGlob a (Type' a) (Item' a)
    | DeclFInv a (Type' a) (Item' a) [Invar' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Arg = Arg' BNFC'Position
data Arg' a
    = Arg a (Type' a) MIdent
    | ArgRef a (Type' a) MIdent
    | ArgFunc a (Type' a) MIdent
    | ArgFuncF a (Type' a) MIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Stmt = Stmt' BNFC'Position
data Stmt' a
    = FnStmt a (Type' a) MIdent [Arg' a] [Stmt' a]
    | FnRetStmt a (Type' a) MIdent [Arg' a] [Stmt' a]
    | Decl a (Type' a) (Item' a)
    | DeclInv a (Type' a) (Item' a) [Invar' a]
    | DeclFunc a (Type' a) MIdent
    | DeclFunF a (Type' a) MIdent
    | Empty a
    | DecLVal a (Type' a) MIdent [Expr' a]
    | DecLSize a (Type' a) MIdent Numb
    | DecLVSize a (Type' a) MIdent Numb (Expr' a)
    | FreeStmt a [Stmt' a]
    | Ass a MIdent (Expr' a)
    | Incr a MIdent EInc
    | Decr a MIdent EDec
    | Ret a (Expr' a)
    | VRet a
    | Cond a (Expr' a) [Stmt' a]
    | CondElse a (Expr' a) [Stmt' a] [Stmt' a]
    | CondElIf a (Expr' a) [Stmt' a] (Expr' a) [Stmt' a]
    | CondElEs a (Expr' a) [Stmt' a] (Expr' a) [Stmt' a] [Stmt' a]
    | While a (Expr' a) [Stmt' a]
    | EBreak a
    | EContinue a
    | SExp a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Invar = Invar' BNFC'Position
data Invar' a = Cannot a (Expr' a) | Must a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Item = Item' BNFC'Position
data Item' a = NoInit a MIdent | Init a MIdent (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Expr = Expr' BNFC'Position
data Expr' a
    = EVar a MIdent
    | ELitInt a Numb
    | ELitTrue a
    | ELitFalse a
    | EApp a MIdent [Expr' a]
    | EIdx a MIdent (Expr' a)
    | EString a String
    | EAnon a [Arg' a] [Stmt' a]
    | Neg a (Expr' a)
    | Not a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type AddOp = AddOp' BNFC'Position
data AddOp' a = Plus a | Minus a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type MulOp = MulOp' BNFC'Position
data MulOp' a = Times a | Div a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type RelOp = RelOp' BNFC'Position
data RelOp' a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

newtype MIdent = MIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Numb = Numb String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype EInc = EInc String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype EDec = EDec String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition Type where
  hasPosition = \case
    Int p -> p
    Str p -> p
    Bool p -> p
    Void p -> p

instance HasPosition Program where
  hasPosition = \case
    Program p _ -> p

instance HasPosition TopDef where
  hasPosition = \case
    FnDef p _ _ _ _ -> p
    FnDefRet p _ _ _ _ -> p
    DeclGlob p _ _ -> p
    DeclFInv p _ _ _ -> p

instance HasPosition Arg where
  hasPosition = \case
    Arg p _ _ -> p
    ArgRef p _ _ -> p
    ArgFunc p _ _ -> p
    ArgFuncF p _ _ -> p

instance HasPosition Stmt where
  hasPosition = \case
    FnStmt p _ _ _ _ -> p
    FnRetStmt p _ _ _ _ -> p
    Decl p _ _ -> p
    DeclInv p _ _ _ -> p
    DeclFunc p _ _ -> p
    DeclFunF p _ _ -> p
    Empty p -> p
    DecLVal p _ _ _ -> p
    DecLSize p _ _ _ -> p
    DecLVSize p _ _ _ _ -> p
    FreeStmt p _ -> p
    Ass p _ _ -> p
    Incr p _ _ -> p
    Decr p _ _ -> p
    Ret p _ -> p
    VRet p -> p
    Cond p _ _ -> p
    CondElse p _ _ _ -> p
    CondElIf p _ _ _ _ -> p
    CondElEs p _ _ _ _ _ -> p
    While p _ _ -> p
    EBreak p -> p
    EContinue p -> p
    SExp p _ -> p

instance HasPosition Invar where
  hasPosition = \case
    Cannot p _ -> p
    Must p _ -> p

instance HasPosition Item where
  hasPosition = \case
    NoInit p _ -> p
    Init p _ _ -> p

instance HasPosition Expr where
  hasPosition = \case
    EVar p _ -> p
    ELitInt p _ -> p
    ELitTrue p -> p
    ELitFalse p -> p
    EApp p _ _ -> p
    EIdx p _ _ -> p
    EString p _ -> p
    EAnon p _ _ -> p
    Neg p _ -> p
    Not p _ -> p
    EMul p _ _ _ -> p
    EAdd p _ _ _ -> p
    ERel p _ _ _ -> p
    EAnd p _ _ -> p
    EOr p _ _ -> p

instance HasPosition AddOp where
  hasPosition = \case
    Plus p -> p
    Minus p -> p

instance HasPosition MulOp where
  hasPosition = \case
    Times p -> p
    Div p -> p

instance HasPosition RelOp where
  hasPosition = \case
    LTH p -> p
    LE p -> p
    GTH p -> p
    GE p -> p
    EQU p -> p
    NE p -> p


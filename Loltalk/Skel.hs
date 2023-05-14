-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Loltalk.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Loltalk.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transMIdent :: Loltalk.Abs.MIdent -> Result
transMIdent x = case x of
  Loltalk.Abs.MIdent string -> failure x

transNumb :: Loltalk.Abs.Numb -> Result
transNumb x = case x of
  Loltalk.Abs.Numb string -> failure x

transEInc :: Loltalk.Abs.EInc -> Result
transEInc x = case x of
  Loltalk.Abs.EInc string -> failure x

transEDec :: Loltalk.Abs.EDec -> Result
transEDec x = case x of
  Loltalk.Abs.EDec string -> failure x

transType :: Show a => Loltalk.Abs.Type' a -> Result
transType x = case x of
  Loltalk.Abs.Int _ -> failure x
  Loltalk.Abs.Str _ -> failure x
  Loltalk.Abs.Bool _ -> failure x
  Loltalk.Abs.Void _ -> failure x

transProgram :: Show a => Loltalk.Abs.Program' a -> Result
transProgram x = case x of
  Loltalk.Abs.Program _ topdefs -> failure x

transTopDef :: Show a => Loltalk.Abs.TopDef' a -> Result
transTopDef x = case x of
  Loltalk.Abs.FnDef _ type_ mident args stmts -> failure x
  Loltalk.Abs.FnDefRet _ type_ mident args stmts -> failure x
  Loltalk.Abs.DeclGlob _ type_ item -> failure x
  Loltalk.Abs.DeclFInv _ type_ item invars -> failure x

transArg :: Show a => Loltalk.Abs.Arg' a -> Result
transArg x = case x of
  Loltalk.Abs.Arg _ type_ mident -> failure x
  Loltalk.Abs.ArgRef _ type_ mident -> failure x
  Loltalk.Abs.ArgFunc _ type_ mident -> failure x
  Loltalk.Abs.ArgFuncF _ type_ mident -> failure x

transStmt :: Show a => Loltalk.Abs.Stmt' a -> Result
transStmt x = case x of
  Loltalk.Abs.FnStmt _ type_ mident args stmts -> failure x
  Loltalk.Abs.FnRetStmt _ type_ mident args stmts -> failure x
  Loltalk.Abs.Decl _ type_ item -> failure x
  Loltalk.Abs.DeclInv _ type_ item invars -> failure x
  Loltalk.Abs.DeclFunc _ type_ mident -> failure x
  Loltalk.Abs.DeclFunF _ type_ mident -> failure x
  Loltalk.Abs.Empty _ -> failure x
  Loltalk.Abs.DecLVal _ type_ mident exprs -> failure x
  Loltalk.Abs.DecLSize _ type_ mident numb -> failure x
  Loltalk.Abs.DecLVSize _ type_ mident numb expr -> failure x
  Loltalk.Abs.FreeStmt _ stmts -> failure x
  Loltalk.Abs.Ass _ mident expr -> failure x
  Loltalk.Abs.Incr _ mident einc -> failure x
  Loltalk.Abs.Decr _ mident edec -> failure x
  Loltalk.Abs.Ret _ expr -> failure x
  Loltalk.Abs.VRet _ -> failure x
  Loltalk.Abs.Cond _ expr stmts -> failure x
  Loltalk.Abs.CondElse _ expr stmts1 stmts2 -> failure x
  Loltalk.Abs.CondElIf _ expr1 stmts1 expr2 stmts2 -> failure x
  Loltalk.Abs.CondElEs _ expr1 stmts1 expr2 stmts2 stmts3 -> failure x
  Loltalk.Abs.While _ expr stmts -> failure x
  Loltalk.Abs.EBreak _ -> failure x
  Loltalk.Abs.EContinue _ -> failure x
  Loltalk.Abs.SExp _ expr -> failure x

transInvar :: Show a => Loltalk.Abs.Invar' a -> Result
transInvar x = case x of
  Loltalk.Abs.Cannot _ expr -> failure x
  Loltalk.Abs.Must _ expr -> failure x

transItem :: Show a => Loltalk.Abs.Item' a -> Result
transItem x = case x of
  Loltalk.Abs.NoInit _ mident -> failure x
  Loltalk.Abs.Init _ mident expr -> failure x

transExpr :: Show a => Loltalk.Abs.Expr' a -> Result
transExpr x = case x of
  Loltalk.Abs.EVar _ mident -> failure x
  Loltalk.Abs.ELitInt _ numb -> failure x
  Loltalk.Abs.ELitTrue _ -> failure x
  Loltalk.Abs.ELitFalse _ -> failure x
  Loltalk.Abs.EApp _ mident exprs -> failure x
  Loltalk.Abs.EIdx _ mident expr -> failure x
  Loltalk.Abs.EString _ string -> failure x
  Loltalk.Abs.EAnon _ args stmts -> failure x
  Loltalk.Abs.Neg _ expr -> failure x
  Loltalk.Abs.Not _ expr -> failure x
  Loltalk.Abs.EMul _ expr1 mulop expr2 -> failure x
  Loltalk.Abs.EAdd _ expr1 addop expr2 -> failure x
  Loltalk.Abs.ERel _ expr1 relop expr2 -> failure x
  Loltalk.Abs.EAnd _ expr1 expr2 -> failure x
  Loltalk.Abs.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => Loltalk.Abs.AddOp' a -> Result
transAddOp x = case x of
  Loltalk.Abs.Plus _ -> failure x
  Loltalk.Abs.Minus _ -> failure x

transMulOp :: Show a => Loltalk.Abs.MulOp' a -> Result
transMulOp x = case x of
  Loltalk.Abs.Times _ -> failure x
  Loltalk.Abs.Div _ -> failure x

transRelOp :: Show a => Loltalk.Abs.RelOp' a -> Result
transRelOp x = case x of
  Loltalk.Abs.LTH _ -> failure x
  Loltalk.Abs.LE _ -> failure x
  Loltalk.Abs.GTH _ -> failure x
  Loltalk.Abs.GE _ -> failure x
  Loltalk.Abs.EQU _ -> failure x
  Loltalk.Abs.NE _ -> failure x

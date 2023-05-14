{-# LANGUAGE FlexibleContexts #-}

import Loltalk.Abs
import Loltalk.Lex
import Loltalk.Par

import System.IO
import System.Environment

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except


main :: IO () 
main = do
    args <- getArgs
    case args of
        [] -> hGetContents stdin >>= parseFile >>= printReturnCode
        [filename] -> readFile filename >>= parseFile >>= printReturnCode

getFileStrinContent :: FilePath -> IO (Either String Value)
getFileStrinContent path = readFile path >>= parseFile

parseFile :: String -> IO (Either String Value)
parseFile fileContent =
    let
      tokens = myLexer fileContent
      parsed = pProgram tokens
    in executeProgram parsed

printReturnCode :: Either String Value -> IO ()
printReturnCode res = do
    case res of
        Left mes -> hPutStrLn stderr mes
        Right cont -> print $ "Exit code: " ++ show cont 

display_tokens :: [Token] -> IO()
display_tokens tokens =  do 
  let 
    parsed = pProgram tokens in
      print parsed


data Value = IntV Int | Success | StringV String | BoolV Bool 
             | FnDefV [Arg] [Stmt] Env | FnDefRetV [Arg] [Stmt] Env 
             | DeclGlobV | DeclFInvV | NonInitV | BreakV | ContinueV | VoidV 

instance Show Value where
    show (IntV v) = show v
    show (StringV v) = show v
    show (BoolV v) = show v
    show Success = "Success"
    show (FnDefV a b c) = "FnDefV " ++ show a ++ " " ++ show b ++ " " ++ show c
    show (FnDefRetV a b c) = "FnDefRetV " ++ show a ++ " " ++ show b ++ " " ++ show c
    show DeclGlobV = "DeclGlobV"
    show DeclFInvV = "DeclFInvV"
    show NonInitV = "NonInitV"
    show BreakV = "BreakV"
    show ContinueV = "ContinueV"
    show VoidV = "VoidV"

-- Store przechowuje wszystkie zmienne przez cały czas
-- Env wskazuje na lokacje aktualnie widocznych zmiennych
-- Env -> Store -> Either String (a, Store)

type Loc = Int
type Env = Map.Map String Loc
data Store = Store {
    store :: Map.Map Loc Value,
    lastLoc :: Loc
} deriving (Show)

type InterpreterMonad a = ReaderT Env (StateT Store (ExceptT String IO)) a 

-- Allocate new location in store and return new location
alloc :: InterpreterMonad Loc
alloc = do
    cur_state <- get
    put cur_state {lastLoc = lastLoc cur_state + 1}
    return (lastLoc cur_state + 1) -- refers to the variable lastLoc

-- Assign the value to the given location
insertToStore val newloc = do
    cur_state <- get
    put cur_state {store = Map.insert newloc val (store cur_state)}

evalMaybe :: String -> Maybe a -> InterpreterMonad a
evalMaybe s Nothing = throwError s
evalMaybe s (Just a) = return a

getEitherMessage (Left mes) = mes
getEitherMessage (Right mes) = mes

executeProgram :: Either String Program -> IO (Either String Value)
executeProgram program = 
    case program of
        Left mes -> runExceptT $ throwError mes
        Right p -> runExceptT $ evalStateT (runReaderT (executeRightProgram p) Map.empty) (Store {store = Map.empty, lastLoc = 0})

executeRightProgram :: Program -> InterpreterMonad Value
executeRightProgram (Program pos topDefs) = do
    updated_env <- evalTopDef topDefs

    local (const updated_env) (eval (EApp pos (MIdent "MAIN") [])) 

printMes mes = lift $ lift $ lift $ putStrLn mes

assignLocVal :: Value -> InterpreterMonad()
assignLocVal funValue = do
    func_loc <- alloc
    insertToStore funValue func_loc

    
-- Declaration of a function which does not return a function
evalTopDef :: [TopDef] -> InterpreterMonad Env

evalTopDef [] = do
    cur_env <- ask
    return cur_env

evalTopDef ((FnDef pos rettype (MIdent ident) args stmts) : rest) = do
    cur_env <- ask
    func_loc <- alloc

    let funValue = (FnDefV args stmts (Map.insert ident func_loc cur_env))
    insertToStore funValue func_loc

    local (Map.insert ident func_loc) (evalTopDef rest)

evalTopDef ((FnDefRet pos rettype (MIdent ident) args stmts) : rest) = do
    cur_env <- ask
    func_loc <- alloc
    
    let funValue = (FnDefRetV args stmts (Map.insert ident func_loc cur_env))
    
    insertToStore funValue func_loc

    local (Map.insert ident func_loc) (evalTopDef rest)

evalTopDef ((DeclGlob pos vartype (NoInit posIn (MIdent ident))) : rest) = do
    var_loc <- alloc
    insertToStore NonInitV var_loc

    local (Map.insert ident var_loc) (evalTopDef rest)


evalTopDef ((DeclGlob pos vartype (Init posIn (MIdent ident) expr)) : rest) = do
    var_loc <- alloc
    var_val <- eval expr
    insertToStore var_val var_loc

    local (Map.insert ident var_loc) (evalTopDef rest)

getValue [] val = val
getValue (x : xs) val
  | x == 'x' = getValue xs (val - 1)
  | otherwise = getValue xs (val + 1)

getLocFromEnv name = do
    var_loc <- asks (Map.lookup name)
    case var_loc of
      Nothing -> throwError $ "Nieznany identyfikator: " ++ name
      Just res -> return res

getValFromStore name = do
    loc <- getLocFromEnv name
    var_val <- gets (Map.lookup loc . store) 
    case var_val of
      Nothing -> throwError $ "Brak wartości zmiennej: " ++ name
      Just NonInitV -> throwError $ "Niezainizjalizowana zmienna: " ++ name
      Just res -> return res

checkIfStringsEqual :: String -> String -> Bool
checkIfStringsEqual [] [] = True
checkIfStringsEqual [] _ = False
checkIfStringsEqual _ [] = False
checkIfStringsEqual (s1 : str1) (s2 : str2)
  | s1 == s2 = checkIfStringsEqual str1 str2
  | otherwise = False

builtinNames = ["FLEX_INT", "FLEX_BOOL", "FLEX_STR"]

checkIfAnyNameFromList :: [String] -> String -> Bool
checkIfAnyNameFromList [] _ = False
checkIfAnyNameFromList (s : ss) name
  | checkIfStringsEqual s name = True
  | otherwise = checkIfAnyNameFromList ss name

isBuilitinFunc funcName = checkIfAnyNameFromList builtinNames funcName

getVariableIdent funcName var = do
    case var of
        (EVar a (MIdent name)) -> return name
        _ -> throwError $ funcName ++ "Not a variable: " ++ show var

getArgLocation :: Expr -> InterpreterMonad Loc
getArgLocation arg = do
    case arg of
        (EVar a (MIdent name)) -> getLocFromEnv name
        _ -> return 0

insertArgsToEnv _ [] [] _ = do 
  updatedEnv <- ask
  return updatedEnv

insertArgsToEnv name [] _ _ = throwError $ "Too few arguments: " ++ name
insertArgsToEnv name _ [] _ = throwError $ "Too many arguments: " ++ name

-- without reference
insertArgsToEnv name (arg_val : rest_vals) ((Arg pos argType (MIdent argName)) : rest_args) (al : passedLocs) = do --funcEnv = do
    new_loc <- alloc
    insertToStore arg_val new_loc

    local (Map.insert argName new_loc) (insertArgsToEnv name rest_vals rest_args passedLocs)

-- with reference
insertArgsToEnv name (arg_val : rest_vals) ((ArgRef pos argType (MIdent argName)) : rest_args) (argLoc : passedLocs) = do --funcEnv = do
    insertToStore arg_val argLoc

    local (Map.insert argName argLoc) (insertArgsToEnv name rest_vals rest_args passedLocs)

getFuncEnv (FnDefV _ _ funcEnv) = funcEnv
getFuncArgs (FnDefV funcArgs _ _) = funcArgs
getFuncBody (FnDefV _ stmts _) = stmts

getBoolFromValue (BoolV v) = v
getIntFromValue (IntV v) = v
getStrFromValue (StringV v) = v

getIncrementVal [] val = val
getIncrementVal (s: ss) val = 
    case s of
        ':' -> getIncrementVal ss 0
        '>' -> getIncrementVal ss (val + 1)

exprMathFuncApply func expr1 expr2 = do
    expr1Val <- eval expr1
    expr2Val <- eval expr2
    let intVal1 = getIntFromValue expr1Val
    let intVal2 = getIntFromValue expr2Val

    let res = func intVal1 intVal2

    return $ IntV res

exprCompApply comp expr1 expr2 = do
    expr1Val <- eval expr1
    expr2Val <- eval expr2
    let int1Val = getIntFromValue expr1Val
    let int2Val = getIntFromValue expr2Val

    let res = comp int1Val int2Val

    return $ BoolV res


eval :: Expr -> InterpreterMonad Value
eval (ELitInt pos (Numb toParse)) = do
    let actualVal = getValue toParse 0
    return $ IntV actualVal

eval (ELitTrue pos) = do
    return $ BoolV True

eval (ELitFalse pos) = do
    return $ BoolV False

eval (EString pos content) = do
    return $ StringV content

eval (EVar pos (MIdent name)) = getValFromStore name

eval (EApp pos (MIdent name) args) = do
  -- current environment - from the moment of the function call
  evaluated_args <- mapM eval args
  argsLocations <- mapM getArgLocation args

  if isBuilitinFunc name
  then
    do
      printMes $ show evaluated_args
      return $ VoidV
  else
    do
        {-
        FnDefV [Arg] [Stmt] Env
        or
        FnDefRetV [Arg] [Stmt] Env 
        At this moment the program will handle the first one
        -}
      funcData <- getValFromStore name
      let funcEnv = getFuncEnv funcData
      let funcArgs = getFuncArgs funcData
      let funcBody = getFuncBody funcData

      updatedFuncEnv <- local (const funcEnv) (insertArgsToEnv name evaluated_args funcArgs argsLocations)

      local (const updatedFuncEnv) (exec funcBody)

eval (Neg pos expr) = do
    exprVal <- eval expr
    return $ IntV (-(getIntFromValue exprVal))

eval (Not pos expr) = do
    exprVal <- eval expr
    return $ BoolV (not (getBoolFromValue exprVal))

eval (EMul pos expr1 (Times posT) expr2) = exprMathFuncApply (*) expr1 expr2

eval (EMul pos expr1 (Div posD) expr2) = do
    expr2Val <- eval expr2
    let intVal2 = getIntFromValue expr2Val

    if (intVal2 == 0)
    then
        throwError "Dzielenie przez zero"
    else
      do
        expr1Val <- eval expr1
        let intVal1 = getIntFromValue expr1Val

        return $ (IntV (div intVal1 intVal2))

eval (EAdd pos expr1 (Plus posP) expr2) = exprMathFuncApply (+) expr1 expr2

eval (EAdd pos expr1 (Minus posM) expr2) = exprMathFuncApply (-) expr1 expr2

eval (ERel pos expr1 operand expr2) = do
    case operand of
        (LTH p) -> exprCompApply (<) expr1 expr2
        (LE p) -> exprCompApply (<=) expr1 expr2
        (GTH p) -> exprCompApply (>) expr1 expr2
        (GE p) -> exprCompApply (>=) expr1 expr2
        (EQU p) -> exprCompApply (==) expr1 expr2
        (NE p) -> exprCompApply (/=) expr1 expr2

eval (EAnd pos expr1 expr2) = do
    expr1Val <- eval expr1

    if (getBoolFromValue expr1Val)
    then
      do
        expr2Val <- eval expr2
        if (getBoolFromValue expr2Val)
        then
          return $ BoolV True
        else
          return $ BoolV False
    else
      return $ BoolV False

eval (EOr pos expr1 expr2) = do
    expr1Val <- eval expr1
    if (getBoolFromValue expr1Val)
    then
      return $ BoolV True
    else
      eval expr2

-- Statements
exec [] = return Success    

exec ((Empty pos) : rest) = exec rest

exec ((Decl pos vartype (NoInit posIn (MIdent ident))) : rest) = do
    var_loc <- alloc
    insertToStore NonInitV var_loc

    local (Map.insert ident var_loc) (exec rest)

exec ((Decl pos vartype (Init posIn (MIdent ident) expr)) : rest) = do
    var_loc <- alloc
    var_val <- eval expr
    insertToStore var_val var_loc

    local (Map.insert ident var_loc) (exec rest)

exec ((SExp pos expr) : rest) = eval expr >> exec rest;

exec ((Ret pos expr) : rest) =  eval expr >>= return;

exec ((VRet pos) : rest) = return VoidV

exec ((Ass pos (MIdent ident) expr) : rest) = do
    var_loc <- getLocFromEnv ident
    var_val <- eval expr
    insertToStore var_val var_loc

    exec rest

exec w@((While pos condExpr whileBody) : rest) = do
    condVal <- eval condExpr
    if (getBoolFromValue condVal)
    then
      do
        bodyRes <- exec whileBody
        case bodyRes of
            BreakV -> exec rest
            ContinueV -> exec w
            Success -> exec w -- the loop has been finished
            VoidV -> exec w
            _ -> return bodyRes
    else
      exec rest

exec ((EBreak pos) : rest) = return BreakV

exec ((EContinue pos) : rest) = return ContinueV

-- if
exec ((Cond pos condExpr stmts) : rest) = do
    condVal <- eval condExpr
    if (getBoolFromValue condVal)
    then
      do
        execRes <- exec stmts
        case execRes of
            Success -> exec rest
            VoidV -> exec rest
            res -> return res
    else
        exec rest

-- if elif
exec ((CondElIf pos condIf stmtsIf condElif stmtsElif) : rest) = do
    condIfVal <- eval condIf
    if (getBoolFromValue condIfVal)
    then
      do
        execRes <- exec stmtsIf
        case execRes of
            Success -> exec rest
            VoidV -> exec rest
            res -> return res
    else
      do
        condElifVal <- eval condElif
        if (getBoolFromValue condElifVal)
        then
          do
            execElif <- exec stmtsElif
            case execElif of
                Success -> exec rest
                VoidV -> exec rest
                res -> return res
        else
          exec rest

-- if else
exec ((CondElse pos condIf stmtsIf stmtsElse) : rest) = do
    condIfVal <- eval condIf
    if (getBoolFromValue condIfVal)
    then
      do
        execRes <- exec stmtsIf
        case execRes of
            Success -> exec rest
            VoidV -> exec rest
            res -> return res
    else
      do
        execElse <- exec stmtsElse
        case execElse of
            Success -> exec rest
            VoidV -> exec rest
            res -> return res

-- if elif else
exec ((CondElEs pos condIf stmtsIf condElif stmtsElif stmtsElse) : rest) = do
    condIfVal <- eval condIf
    if (getBoolFromValue condIfVal)
    then
      do
        execIf <- exec stmtsIf
        case execIf of
            Success -> exec rest
            VoidV -> exec rest
            res -> return res
    else
      do
        condElifVal <- eval condElif
        if (getBoolFromValue condElifVal)
        then
          do
            execElif <- exec stmtsElif
            case execElif of
                Success -> exec rest
                VoidV -> exec rest
                res -> return res
        else
          do
            execElse <- exec stmtsElse
            case execElse of
                Success -> exec rest
                VoidV -> exec rest
                res -> return res
module TypeChecker ( typecheck ) where

import AbsCPP
import ErrM
import PrintCPP

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.List ( intercalate, intersperse )
import Control.Monad ( foldM, foldM_, forM_, unless )


newtype Alternative a = Alternative [a]
instance Print a => Print (Alternative a) where
    prt i (Alternative xs) = 
        ((foldr (.) id) . (intersperse (doc (showString "/"))) . map (prt i)) xs

--Type Mismatch error prints out the type error found
typeMismatchError :: (Print e, Print t1, Print t2) => e -> t1 -> t2 -> String
typeMismatchError e tExp tFound = 
    "TYPE ERROR\n\n" ++
    "Expected " ++ printTree e ++ " to have type " ++ printTree tExp ++
    " instead found type " ++ printTree tFound ++ "."

--ok prints out when there is no type mismatch error
ok :: Err ()
ok = Ok ()


type FunctionType = ([Type], Type)
type Sig = Map Id FunctionType
type Context = Map Id Type
type Env = (Sig, [Context])

--lookupFun takes in an environment and ID (function name) and returns either an 
--error or the type of the function name in the environment
lookupFun :: Env -> Id -> Err FunctionType
lookupFun (sig,_) id = case M.lookup id sig of
    Just ty -> return ty
    Nothing -> fail $ "TYPE ERROR\n\n" ++ printTree id ++ " was not declared."

--InsertFun takes in an environment, ID (function name), and a type and returns an 
--environment in which the ID'd function exists as the given type (or an error if already declared)
insertFun :: Env -> Id -> FunctionType -> Err Env
insertFun (sig,ctxt) i t = do
    case M.lookup i sig of
        Just _  -> fail $ 
            "TYPE ERROR\n\nFailed to add " 
            ++ printTree i ++ "to the symbol table, as it is already defined"
        Nothing -> return (M.insert i t sig, ctxt)

--lookupvar takes in an ID (variable name) and an environment and returns either the type
--of that variable in the environemtn or an error if not found
lookupVar :: Id -> Env -> Err Type
lookupVar i (_,[]) = fail $ "TYPE ERROR\n\n" ++ printTree i ++ " was not declared."
lookupVar i (sig,c:ctxt) = case M.lookup i c of
    (Just f) -> return f
    Nothing -> lookupVar i (sig,ctxt)

--insertvar takes in an environment, ID (Variable name), and a type and returns either an error
--if already declared, or the same environment but in which the variable has the given type
insertVar :: Env -> Id -> Type -> Err Env
insertVar (_, []) _ _ = fail $ "Internal error, this should not happen."
insertVar (sig, c:ctxt) i t = 
    case M.lookup i c of
        Just _  -> fail $ 
            "TYPE ERROR\n\nFailed to add " 
            ++ printTree i ++ "to the context, as it is already defined within this block."
        Nothing -> 
            if t == Type_void then
                fail $ "TYPE ERROR\n\nCannot declare variable " ++ printTree i ++ " as void."
            else
                return (sig, (M.insert i t c):ctxt)

--makes new block
newBlock :: Env -> Env
newBlock (sig,ctxt) = (sig, M.empty:ctxt)

--creates empty environment
emptyEnv :: Env
emptyEnv = (M.fromList 
    [ 
        (Id "printInt",    ([Type_int],    Type_void))
      , (Id "printDouble", ([Type_double], Type_void))
      , (Id "readInt",     ([],            Type_int))
      , (Id "readDouble",  ([],            Type_double))
    ], [M.empty])

--builds the environment with everytin in iy
buildEnv :: [Def] -> Err Env
buildEnv [] = return emptyEnv
buildEnv (DFun t i arg _:xs) = do
    env <- buildEnv xs
    insertFun env i (map (\(ADecl t _) -> t) arg, t) 

--checks the file being empty and builds an environment then 
typecheck :: Program -> Err ()
typecheck (PDefs []) = fail $ "TYPE ERROR\n\nFile cannot be empty."
typecheck (PDefs defs) = do
    env <- buildEnv defs
    forM_ defs (checkDef env)

--
checkDef :: Env -> Def -> Err ()
checkDef env (DFun ty (Id n) args stms) = do
    if (n == "main") then checkMain ty args else ok
    env' <- foldM (\e (ADecl ty' i) -> insertVar e i ty') env args
    foldM_ (\e s -> checkStm e s ty) env' stms

--makes sure main has no args
checkMain :: Type -> [Arg] -> Err ()
checkMain Type_int [] = ok
checkMain Type_int xs = fail $ "TYPE ERROR\n\nError, main cannot have arguments."
checkMain ty _ = fail $ typeMismatchError (Id "main") Type_int ty

--checks args of statements and returns environment
checkStm :: Env -> Stm -> Type -> Err Env 
checkStm env (SExp e) ty = do
    inferTypeExp env e
    return env
checkStm env (SDecls ty' ids) ty =
    foldM (\e i -> insertVar e i ty') env ids
checkStm env (SReturn e) ty = do
    checkExp env e ty
    return env
checkStm env (SInit ty' id e) ty = do
    insertVar env id ty
checkStm env (SReturnVoid) ty = do
    return env
checkStm env (SWhile e s) ty = do
    inferTypeExp env e
    checkStm env s ty
    return env
checkStm env (SBlock s) ty = do
    checkStms (newBlock env) s ty
    return env
checkStm env (SIfElse e s1 s2) ty = do
    inferTypeExp env e
    checkStm env s1 ty
    checkStm env s2 ty
    return env
checkStm _ s _ = fail $ "Missing case in checkStm encountered:\n" ++ printTree s

checkStms:: Env -> [Stm] ->Type -> Err Env
checkStms env [] ty = return env
checkStms env (stm:stms) ty = do
    env' <- checkStm env stm ty
    checkStms env' stms ty

{-
Here need to go the missing cases. Once you have all cases you can delete the next line which is only needed to catch all cases that are not yet implemented.
-}

--infer type takes in an expression and an environemnt and returns the inferred type of the expression
inferTypeExp :: Env -> Exp -> Err Type
inferTypeExp env (ETrue) = do
    --i <- lookupvar
    return Type_bool
inferTypeExp env (EFalse) = do
    return Type_bool
inferTypeExp env (EInt _) = do
    return Type_int
inferTypeExp env (EDouble _) = do
    return Type_double
inferTypeExp env (EString _) = do
    return Type_string
inferTypeExp env (EId id) = do
    i <- lookupVar id env 
    return i
inferTypeExp env (EApp _ es) = do
    return Type_bool
inferTypeExp env (EPIncr e) = do 
    inferTypeExp env e
inferTypeExp env (EPDecr e) = do
    inferTypeExp env e
inferTypeExp env (EIncr e) = do
    inferTypeExp env e
inferTypeExp env (EDecr e) = do
    inferTypeExp env e
inferTypeExp env (ETimes e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_int,Type_double]) e1 [e2]
inferTypeExp env (EDiv e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_int,Type_double]) e1 [e2]
inferTypeExp env (EPlus e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_int,Type_double]) e1 [e2]
inferTypeExp env (EMinus e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_int,Type_double]) e1 [e2]
inferTypeExp env (ELt e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_bool]) e1 [e2]
inferTypeExp env (EGt e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_bool]) e1 [e2]
inferTypeExp env (ELtEq e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_bool]) e1 [e2]
inferTypeExp env (EGtEq e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_bool]) e1 [e2]
inferTypeExp env (EEq e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_int,Type_double,Type_string,Type_bool]) e1 [e2]
inferTypeExp env (ENEq e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_int,Type_double,Type_string,Type_bool]) e1 [e2]
inferTypeExp env (EAnd e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_bool]) e1 [e2]
inferTypeExp env (EOr e1 e2) = do
    inferTypeOverloadedExp env (Alternative [Type_bool]) e1 [e2]
inferTypeExp env (EAss e1 e2) = do
    ty <- inferTypeExp env e1
    checkExp env e2 ty
    return ty
inferTypeExp env (ETyped e ty) = do
    checkExp env e ty
    return ty
{-
Here need to go the missing cases. Once you have all cases you can delete the next line which is only needed to catch all cases that are not yet implemented.
-}
inferTypeExp _ e = fail $ "Missing case in inferTypeExp encountered:\n" ++ printTree e

--infer type of first expression, and if that type isn't one of the alternate types it fails
--then checkExp of all other expressions using the type of the first expression
inferTypeOverloadedExp :: Env -> Alternative Type -> Exp -> [Exp] -> Err Type
inferTypeOverloadedExp env (Alternative ts) e es = do
    ty <- inferTypeExp env e
    unless (ty `elem` ts) $ 
        fail $ typeMismatchError e (Alternative ts) ty
    forM_ es (flip (checkExp env) ty)
    return ty

--checkExp takes in an environemnt, expression, type, and returns either an error 
--or nothing meaning the expression in the environment has the correct type
checkExp :: Env -> Exp -> Type -> Err ()
checkExp env e ty = do
    ty' <- inferTypeExp env e
    unless (ty == ty') $ 
        fail $ typeMismatchError e ty ty'
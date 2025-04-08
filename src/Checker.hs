module Checker (
    SymbolTable,
    checkProgram
) where

import Data.Map hiding (map)

import Stella.Abs

import CheckError

type SymbolTable = Map String Type;

data Context = Context
    { table :: SymbolTable
    , extensions :: [String]
    }
    deriving (Eq, Show);

scanExtensions :: Program -> CheckResult [String]
scanExtensions (AProgram _ exts decls) = return (exts >>= scanExtPhrase)
  where
    scanExtPhrase (AnExtension names) = map scanExtName names
    scanExtName (ExtensionName name) = name

scanFunctions :: Program -> CheckResult SymbolTable
scanFunctions (AProgram _ _ decls) = scanFunctionDecls decls

scanFunctionDecls :: [Decl] -> CheckResult SymbolTable
scanFunctionDecls = fmap fromList . mapM scanFunctionDecl

scanFunctionDecl :: Decl -> CheckResult (String, Type)
scanFunctionDecl (DeclFun _ (StellaIdent name) args (NoReturnType) _ _ _)
    = return (name, TypeFun (paramsToTypes args) TypeUnit)
scanFunctionDecl (DeclFun _ (StellaIdent name) args (SomeReturnType retType) _ _ _)
    = return (name, TypeFun (paramsToTypes args) retType)
scanFunctionDecl _ = checkFailed Unsupported " Unsupported"

paramsToTypes :: [ParamDecl] -> [Type]
paramsToTypes [] = []
paramsToTypes (AParamDecl _ _type : next) = _type : paramsToTypes next

checkMainExistence :: Context -> CheckResult ()
checkMainExistence ctx =
    if hasMain $ keys $ table ctx
        then return ()
        else checkFailed MissingMain "  Отсутствует функция Main"
  where
    hasMain [] = False
    hasMain ("main" : _) = True
    hasMain (_ : rest) = hasMain rest

checkExtensionSupport :: Context -> CheckResult ()
checkExtensionSupport ctx = mapM_ checkExtension $ extensions ctx
  where
    checkExtension :: String -> CheckResult ()
    checkExtension "#unit-type"                    = return ()
    checkExtension "#unit-type"                    = return ()
    checkExtension "#pairs"                        = return ()
    checkExtension "#tuples"                       = return ()
    checkExtension "#records"                      = return ()
    checkExtension "#let-bindings"                 = return ()
    checkExtension "#type-ascriptions"             = return ()
    checkExtension "#sum-types"                    = return ()
    checkExtension "#lists"                        = return ()
    checkExtension "#variants"                     = return ()
    checkExtension "#fixpoint-combinator"          = return ()
    checkExtension "#natural-literals"             = return ()
    --checkExtension "#nested-function-declarations" = return ()
    --checkExtension "#nullary-functions"            = return ()
    --checkExtension "#multiparameter-functions"     = return ()
    --checkExtension "#structural-patterns"          = return ()
    --checkExtension "#nullary-variant-labels"       = return ()
    --checkExtension "#letrec-bindings"              = return ()
    --checkExtension "#pattern-ascriptions"          = return ()
    checkExtension ext = checkFailed Unsupported $ "  Unsupported extension: " ++ ext

checkExtensionEnabled :: Context -> String -> CheckResult ()
checkExtensionEnabled ctx ext = checkExtensionEnabledInList (extensions ctx) ext
  where
    checkExtensionEnabledInList [] ext = checkFailed Unsupported $ "  Extention not enabled: " ++ ext
    checkExtensionEnabledInList (ext' : next) ext
        | ext' == ext = return ()
        | otherwise   = checkExtensionEnabledInList next ext

getType :: Context -> String -> CheckResult Type
getType ctx name
    = case Data.Map.lookup name (table ctx) of
        Just t -> return t
        Nothing -> checkFailed UndefinedVariable $ "  Undefined variable: " ++ name

assumeType :: Context -> String -> Type -> Context
assumeType ctx name newType = ctx{table = insert name newType (table ctx)}

assumeFunctionParams :: Context -> [ParamDecl] -> Context
assumeFunctionParams ctx [] = ctx
assumeFunctionParams ctx (AParamDecl (StellaIdent name) theType : next)
    = assumeFunctionParams (assumeType ctx name theType) next

checkDeclaration :: Context -> Decl -> CheckResult ()
checkDeclaration ctx (DeclFun _ _ args (NoReturnType) _ _ expr) = do
    let newCtx = assumeFunctionParams ctx args
    checkTypeExpr newCtx TypeUnit expr
checkDeclaration ctx (DeclFun _ _ args (SomeReturnType retType) _ _ expr) = do
    let newCtx = assumeFunctionParams ctx args
    checkTypeExpr newCtx retType expr
checkDeclaration _ _ = checkFailed Unsupported " Unsupported"

checkProgram :: Program -> CheckResult ()
checkProgram prog@(AProgram _ _ decls) = do
    ext <- scanExtensions prog
    tbl <- scanFunctions prog
    let ctx = Context{table = tbl, extensions = ext}
    checkExtensionSupport ctx
    checkMainExistence ctx
    mapM_ (checkDeclaration ctx) decls

{-
expandTypeFunc :: Type -> Type
expandTypeFunc (TypeFun [a] r) = TypeFun [a] r
expandTypeFunc (TypeFun (a:aa) r) = TypeFun [a] $ expandTypeFunc $ TypeFun aa r
expandTypeFunc t = t

equalTypes :: Type -> Type -> Bool
equalTypes (TypeFun [] r1) (TypeFun [] r2) = equalTypes r1 r2
equalTypes (TypeFun [a1] r1) (TypeFun [a2] r2) = (equalTypes a1 a2) && (equalTypes r1 r2)
equalTypes (TypeFun [a1:aa1] r1) (TypeFun [a2] r2) = (equalTypes a1 a2) && (equalTypes (TypeFun aa1 r1) r2)
equalTypes (TypeFun [a1] r1) (TypeFun [a2:aa2] r2) = (equalTypes a1 a2) && (equalTypes r1 (TypeFun aa2 r2))
equalTypes (TypeFun [a1:aa1] r1) (TypeFun [a2:aa2] r2) = (equalTypes a1 a2) && (equalTypes (TypeFun aa1 r1) (TypeFun aa2 r2))
equalTypes a b = a == b
-}

checkTypes :: Type -> Type -> CheckResult ()
checkTypes expected actual = when (expected /= actual) $ checkFailed UnexpectedTypeForExpression $
    "  Expected " ++ show expected ++ ", actual " ++ show actual

-- CHECK

checkTypeExpr :: Context -> Type -> Expr -> CheckResult ()
checkTypeExpr _ctx _sample (Sequence _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Assign _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr ctx sample (If exprCond exprTrue exprFalse) = do
    checkTypeExpr ctx TypeBool exprCond
    checkTypeExpr ctx sample exprTrue
    checkTypeExpr ctx sample exprFalse
checkTypeExpr _ctx _sample (Let _patterns _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (LetRec _patterns _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (TypeAbstraction _names _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (LessThan _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (LessThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (GreaterThan _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (GreaterThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Equal _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (NotEqual _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (TypeAsc _expr _theType) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (TypeCast _expr _theType) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Abstraction _params _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Variant (StellaIdent _name) _exprData) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Match _expr _cases) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (List _exprs) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Add _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Subtract _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (LogicOr _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Multiply _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Divide _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (LogicAnd _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Ref _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Deref _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr ctx sample (Application func args) = do
    let argLen = length args
    when (argLen == 0) $ checkExtensionEnabled ctx "#nullary-functions"
    when (argLen > 1) $ checkExtensionEnabled ctx "#multiparameter-functions"
    t <- synthTypeExpr ctx func
    case t of
        TypeFun argTypes retType ->
            let funArgLen = length argTypes
            when (funArgLen == 0) $ checkExtensionEnabled ctx "#nullary-functions"
            when (funArgLen > 1) $ checkExtensionEnabled ctx "#multiparameter-functions"
            when (funArgLen /= argLen) $ checkFailed IncorrectNumberOfArguments $
                "  Func has " ++ show funArgLen ++ ", provided " ++ show argLen
            zipWithM_ (checkTypeExpr ctx) argTypes args
            checkTypes sample retType
        _ -> checkFailed NotAFunction "  Not a function"
checkTypeExpr _ctx _sample (TypeApplication _expr _types) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (DotRecord _expr (StellaIdent _name)) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (DotTuple _expr _n) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Tuple _exprs) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Record _bindings) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (ConsList _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Head _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (IsEmpty _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Tail _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Panic) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Throw _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (TryCatch _exprTry _pattern _exprCatch) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (TryWith _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (TryCastAs _expr1 _theType _pattern _expr2 _expr3) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Inl _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Inr _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Succ _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (LogicNot _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Pred _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (IsZero _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Fix _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (NatRec _expr1 _expr2 _expr3) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Fold _theType _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (Unfold _theType _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr _ctx _sample (ConstTrue) = do
    checkTypes sample TypeBool
checkTypeExpr _ctx _sample (ConstFalse) = do
    checkTypes sample TypeBool
checkTypeExpr ctx sample (ConstUnit) = do
    checkExtensionEnabled ctx "#unit-type"
    checkTypes sample TypeUnit
checkTypeExpr ctx sample (ConstInt n) = do
    when (n /= 0) $ checkExtensionEnabled ctx "#natural-literals"
    checkTypes sample TypeInt
checkTypeExpr _ctx _sample (ConstMemory (MemoryAddress _address)) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
checkTypeExpr ctx sample (Var (StellaIdent name)) = do
    t <- getType ctx name
    if ctx == _sample
        then return ()
        else case sample of
            TypeFun _ _ -> checkFailed NotAFunction $ "  Checking var " ++ name
            TypeTuple _ -> checkFailed NotATuple $ "  Checking var " ++ name
            TypeRecord _ -> checkFailed NotARecord $ "  Checking var " ++ name
            TypeList _ -> checkFailed NotAList $ "  Checking var " ++ name
            _ -> checkFailed UnexpectedTypeForExpression $ "  Checking var " ++ name

-- SYNTH

synthTypeExpr :: Context -> Expr -> CheckResult Type
synthTypeExpr _ctx (Sequence _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Assign _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr ctx (If exprCond exprTrue exprFalse) = do
    checkTypeExpr ctx TypeBool exprCond
    sample <- synthTypeExpr ctx exprTrue
    checkTypeExpr ctx sample exprFalse
    return sample
synthTypeExpr _ctx (Let _patterns _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (LetRec _patterns _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (TypeAbstraction _names _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (LessThan _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (LessThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (GreaterThan _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (GreaterThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Equal _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (NotEqual _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (TypeAsc _expr _theType) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (TypeCast _expr _theType) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Abstraction _params _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Variant (StellaIdent _name) _exprData) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Match _expr _cases) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (List _exprs) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Add _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Subtract _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (LogicOr _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Multiply _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Divide _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (LogicAnd _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Ref _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Deref _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr ctx (Application func args) = do
    let argLen = length args
    when (argLen == 0) $ checkExtensionEnabled ctx "#nullary-functions"
    when (argLen > 1) $ checkExtensionEnabled ctx "#multiparameter-functions"
    t <- synthTypeExpr ctx func
    case t of
        TypeFun argTypes retType ->
            let funArgLen = length argTypes
            when (funArgLen == 0) $ checkExtensionEnabled ctx "#nullary-functions"
            when (funArgLen > 1) $ checkExtensionEnabled ctx "#multiparameter-functions"
            when (funArgLen /= argLen) $ checkFailed IncorrectNumberOfArguments $
                "  Func has " ++ show funArgLen ++ ", provided " ++ show argLen
            zipWithM_ (checkTypeExpr ctx) argTypes args
            return retType
        _ -> checkFailed NotAFunction "  Not a function"
synthTypeExpr _ctx (TypeApplication _expr _types) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (DotRecord _expr (StellaIdent _name)) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (DotTuple _expr _n) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Tuple _exprs) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Record _bindings) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (ConsList _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Head _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (IsEmpty _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Tail _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Panic) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Throw _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (TryCatch _exprTry _pattern _exprCatch) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (TryWith _expr1 _expr2) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (TryCastAs _expr1 _theType _pattern _expr2 _expr3) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Inl _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Inr _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Succ _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (LogicNot _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Pred _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (IsZero _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Fix _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (NatRec _expr1 _expr2 _expr3) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Fold _theType _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (Unfold _theType _expr) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr _ctx (ConstTrue) = do
    return TypeBool
synthTypeExpr _ctx (ConstFalse) = do
    return TypeBool
synthTypeExpr ctx (ConstUnit) = do
    checkExtensionEnabled ctx "#unit-type"
    return TypeUnit
synthTypeExpr ctx (ConstInt n) = do
    when (n /= 0) $ checkExtensionEnabled ctx "#natural-literals"
    return TypeInt
synthTypeExpr _ctx (ConstMemory (MemoryAddress _address)) = do
    checkFailed Unsupported "  Unsupported"  -- TODO
synthTypeExpr ctx (Var StellaIdent) = do
    getType ctx name

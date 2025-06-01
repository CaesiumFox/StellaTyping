module Checker (
    SymbolTable,
    checkProgram
) where

import Data.Map hiding (map)
import Data.Set hiding (map)
import Control.Monad (when, zipWithM_)

import Stella.Abs

import CheckError
import Named

type SymbolTable = Map String Type;

data Context = Context
    { table :: SymbolTable
    , extensions :: [String]
    }
    deriving (Eq, Show);

scanExtensions :: Program -> CheckResult [String]
scanExtensions (AProgram _ exts _decls) = return (exts >>= scanExtPhrase)
  where
    scanExtPhrase (AnExtension names) = map scanExtName names
    scanExtName (ExtensionName name) = name

scanFunctions :: Program -> CheckResult SymbolTable
scanFunctions (AProgram _ _ decls) = scanFunctionDecls decls

scanFunctionDecls :: [Decl] -> CheckResult SymbolTable
scanFunctionDecls = fmap Data.Map.fromList . mapM scanFunctionDecl

scanFunctionDecl :: Decl -> CheckResult (String, Type)
scanFunctionDecl (DeclFun _ (StellaIdent name) args (NoReturnType) _ _ _)
    = return (name, TypeFun (paramsToTypes args) TypeUnit)
scanFunctionDecl (DeclFun _ (StellaIdent name) args (SomeReturnType retType) _ _ _)
    = return (name, TypeFun (paramsToTypes args) retType)
scanFunctionDecl _ = checkFailed Unsupported

paramsToTypes :: [ParamDecl] -> [Type]
paramsToTypes [] = []
paramsToTypes (AParamDecl _ _type : next) = _type : paramsToTypes next

checkMainExistence :: Context -> CheckResult ()
checkMainExistence ctx =
    if hasMain $ keys $ table ctx
        then return ()
        else checkFailed MissingMain
  where
    hasMain [] = False
    hasMain ("main" : _) = True
    hasMain (_ : rest) = hasMain rest

checkExtensionSupport :: Context -> CheckResult ()
checkExtensionSupport ctx = mapM_ checkExtension $ extensions ctx
  where
    checkExtension :: String -> CheckResult ()
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
    checkExtension "#nullary-functions"            = return ()
    checkExtension "#multiparameter-functions"     = return ()
    checkExtension _ext = checkFailed Unsupported

checkExtensionEnabled :: Context -> String -> CheckResult ()
checkExtensionEnabled ctx extension = checkExtensionEnabledInList (extensions ctx) extension
  where
    checkExtensionEnabledInList [] _ext = checkFailed Unsupported
    checkExtensionEnabledInList (ext' : next) ext
        | ext' == ext = return ()
        | otherwise   = checkExtensionEnabledInList next ext

getType :: Context -> String -> CheckResult Type
getType ctx name
    = case Data.Map.lookup name (table ctx) of
        Just t -> return t
        Nothing -> checkFailed UndefinedVariable

assumeType :: Context -> String -> Type -> Context
assumeType ctx name newType = ctx{table = Data.Map.insert name newType (table ctx)}

assumeTypes :: Context -> [(String, Type)] -> Context
assumeTypes ctx [] = ctx
assumeTypes ctx ((name, newType) : next) = assumeTypes (assumeType ctx name newType) next

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
checkDeclaration _ _ = checkFailed Unsupported

checkProgram :: Program -> CheckResult ()
checkProgram prog@(AProgram _ _ decls) = do
    ext <- scanExtensions prog
    tbl <- scanFunctions prog
    let ctx = Context{table = tbl, extensions = ext}
    checkExtensionSupport ctx
    checkMainExistence ctx
    mapM_ (checkDeclaration ctx) decls

checkTypes :: Context -> Type -> Type -> CheckResult ()
checkTypes ctx = checkTypesWith ctx UnexpectedTypeForExpression

checkTypesWith :: Context -> CheckErrorCode -> Type -> Type -> CheckResult ()
checkTypesWith _ctx err expected actual = when (expected /= actual) $ checkFailed err



-- UTILITY

getAssumptionFromPattern :: Context -> PatternBinding -> CheckResult (String, Type)
getAssumptionFromPattern ctx (APatternBinding pattern expr) = do
    t <- synthTypeExpr ctx expr
    case pattern of
        PatternVar (StellaIdent name) -> return (name, t)
        _ -> checkFailed Unsupported

getRecordFieldType :: Context -> String -> [RecordFieldType] -> CheckResult Type
getRecordFieldType _ctx _expected [] = checkFailed UnexpectedFieldAccess
getRecordFieldType ctx expected ((ARecordFieldType (StellaIdent name) theType) : next) =
    if expected == name
        then return theType
        else getRecordFieldType ctx expected next

getTupleFieldType :: Context -> Integer -> [Type] -> CheckResult Type
getTupleFieldType _ctx _n [] = checkFailed TupleIndexOutOfBounds
getTupleFieldType ctx n (theType : next)
    | n < 1     = checkFailed TupleIndexOutOfBounds
    | n == 1    = return theType
    | otherwise = getTupleFieldType ctx (n - 1) next

checkNameDuplicates' :: CheckErrorCode -> [String] -> CheckResult ()
checkNameDuplicates' code names =
    when (Data.Set.size (Data.Set.fromList names :: (Set String)) /= length names) $
        checkFailed code

checkNameDuplicates :: (Named a) => CheckErrorCode -> [a] -> CheckResult ()
checkNameDuplicates code named = checkNameDuplicates' code $ map getName named



-- CHECK

checkTypeExpr :: Context -> Type -> Expr -> CheckResult ()
checkTypeExpr _ctx _sample (Sequence _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Assign _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (If exprCond exprTrue exprFalse) = do
    checkTypeExpr ctx TypeBool exprCond
    checkTypeExpr ctx sample exprTrue
    checkTypeExpr ctx sample exprFalse
checkTypeExpr ctx sample (Let patterns expr) = do
    assumptions <- mapM (getAssumptionFromPattern ctx) patterns
    let newCtx = assumeTypes ctx assumptions
    checkTypeExpr newCtx sample expr
checkTypeExpr _ctx _sample (LetRec _patterns _expr) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (TypeAbstraction _names _expr) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (LessThan _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (LessThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (GreaterThan _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (GreaterThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Equal _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (NotEqual _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (TypeAsc expr theType) = do
    checkTypes ctx sample theType
    checkTypeExpr ctx theType expr
checkTypeExpr _ctx _sample (TypeCast _expr _theType) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (Abstraction params expr) = do
    case sample of
        TypeFun tParams tRet -> do
            let tParamsLen = length tParams
            let paramsLen = length params
            when (tParamsLen /= paramsLen) $ checkFailed IncorrectNumberOfArguments
            let declaredTypes = map (\(AParamDecl _ t) -> t) params
            zipWithM_ (checkTypes ctx) tParams declaredTypes
            checkTypeExpr (assumeFunctionParams ctx params) tRet expr
        _ -> checkFailed UnexpectedLambda
checkTypeExpr _ctx _sample (Variant (StellaIdent _name) _exprData) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (Match _expr _cases) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (List _exprs) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (Add _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Subtract _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (LogicOr _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Multiply _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Divide _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (LogicAnd _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Ref _expr) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Deref _expr) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (Application func args) = do
    let argLen = length args
    t <- synthTypeExpr ctx func
    case t of
        TypeFun argTypes retType -> do
            let funArgLen = length argTypes
            when (funArgLen /= argLen) $ checkFailed IncorrectNumberOfArguments
            zipWithM_ (checkTypeExpr ctx) argTypes args
            checkTypes ctx sample retType
        _ -> checkFailed NotAFunction
checkTypeExpr _ctx _sample (TypeApplication _expr _types) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (DotRecord expr (StellaIdent name)) = do
    t <- synthTypeExpr ctx expr
    case t of
        TypeRecord fields -> do
            ft <- getRecordFieldType ctx name fields
            checkTypes ctx sample ft
        _ -> checkFailed NotARecord
checkTypeExpr ctx sample (DotTuple expr n) = do
    t <- synthTypeExpr ctx expr
    case t of
        TypeTuple fields -> do
            ft <- getTupleFieldType ctx n fields
            checkTypes ctx sample ft
        _ -> checkFailed NotATuple
checkTypeExpr ctx sample (Tuple exprs) =
    case sample of
        TypeTuple fields -> do
            when (length exprs /= length fields) $ checkFailed UnexpectedTupleLength
            zipWithM_ (checkTypeExpr ctx) fields exprs
        _ -> checkFailed UnexpectedTuple
checkTypeExpr ctx sample (Record bindings) =
    case sample of
        TypeRecord sampleBindings -> do
            when (length bindings /= length sampleBindings) $ checkFailed UnexpectedTupleLength
            -- zipWithM_ (checkTypeExpr ctx) sampleBindings bindings
            -- FIXME: check field missing/unexpected
        _ -> checkFailed UnexpectedTuple
checkTypeExpr _ctx _sample (ConsList _expr1 _expr2) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (Head _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (IsEmpty _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (Tail _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (Panic) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Throw _expr) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (TryCatch _exprTry _pattern _exprCatch) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (TryWith _expr1 _expr2) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (TryCastAs _expr1 _theType _pattern _expr2 _expr3) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Inl _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr _ctx _sample (Inr _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr ctx sample (Succ expr) = do
    checkTypes ctx sample TypeNat
    checkTypeExpr ctx TypeNat expr
checkTypeExpr _ctx _sample (LogicNot _expr) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Pred _expr) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (IsZero expr) = do
    checkTypes ctx sample TypeBool
    checkTypeExpr ctx TypeNat expr
checkTypeExpr ctx sample (Fix expr) = do
    eType <- synthTypeExpr ctx expr
    case eType of
        TypeFun lTypes rType -> do
            when (length lTypes /= 1) $ checkFailed IncorrectNumberOfArguments -- ???
            checkTypes ctx sample rType
            checkTypes ctx rType $ head lType -- ???
        _ -> NotAFunction
    checkFailed Unsupported  -- TODO: first, urgent
checkTypeExpr ctx sample (NatRec count initial step) = do
    checkTypeExpr ctx TypeNat count
    initType <- synthTypeExpr ctx initial
    checkTypes ctx sample initType
    checkTypeExpr ctx (TypeFun [TypeNat] (TypeFun [initType] initType)) step
checkTypeExpr _ctx _sample (Fold _theType _expr) = do
    checkFailed Unsupported
checkTypeExpr _ctx _sample (Unfold _theType _expr) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (ConstTrue) = do
    checkTypes ctx sample TypeBool
checkTypeExpr ctx sample (ConstFalse) = do
    checkTypes ctx sample TypeBool
checkTypeExpr ctx sample (ConstUnit) = do
    checkTypes ctx sample TypeUnit
checkTypeExpr ctx sample (ConstInt _n) = do
    checkTypes ctx sample TypeNat
checkTypeExpr _ctx _sample (ConstMemory (MemoryAddress _address)) = do
    checkFailed Unsupported
checkTypeExpr ctx sample (Var (StellaIdent name)) = do
    t <- getType ctx name
    if t == sample
        then return ()
        else case sample of
            TypeFun _ _ -> checkFailed NotAFunction
            TypeTuple _ -> checkFailed NotATuple
            TypeRecord _ -> checkFailed NotARecord
            TypeList _ -> checkFailed NotAList
            _ -> checkFailed UnexpectedTypeForExpression



-- SYNTH

synthTypeExpr :: Context -> Expr -> CheckResult Type
synthTypeExpr _ctx (Sequence _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Assign _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr ctx (If exprCond exprTrue exprFalse) = do
    checkTypeExpr ctx TypeBool exprCond
    sample <- synthTypeExpr ctx exprTrue
    checkTypeExpr ctx sample exprFalse
    return sample
synthTypeExpr ctx (Let patterns expr) = do
    assumptions <- mapM (getAssumptionFromPattern ctx) patterns
    let newCtx = assumeTypes ctx assumptions
    synthTypeExpr newCtx expr
synthTypeExpr _ctx (LetRec _patterns _expr) = do
    checkFailed Unsupported
synthTypeExpr _ctx (TypeAbstraction _names _expr) = do
    checkFailed Unsupported
synthTypeExpr _ctx (LessThan _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (LessThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (GreaterThan _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (GreaterThanOrEqual _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Equal _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (NotEqual _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr ctx (TypeAsc expr theType) = do
    checkTypeExpr ctx theType expr
    return theType
synthTypeExpr _ctx (TypeCast _expr _theType) = do
    checkFailed Unsupported
synthTypeExpr ctx (Abstraction params expr) = do
    --let paramsLen = length params
    let declaredTypes = map (\(AParamDecl _ t) -> t) params
    retType <- synthTypeExpr (assumeFunctionParams ctx params) expr
    return $ TypeFun declaredTypes retType
synthTypeExpr _ctx (Variant (StellaIdent _name) _exprData) = do
    checkFailed AmbiguousVariantType -- FUTURE: subtyping
synthTypeExpr _ctx (Match _expr _cases) = do
    checkFailed Unsupported  -- TODO: first, urgent
synthTypeExpr _ctx (List _exprs) = do
    checkFailed Unsupported  -- TODO: first, urgent
synthTypeExpr _ctx (Add _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Subtract _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (LogicOr _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Multiply _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Divide _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (LogicAnd _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Ref _expr) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Deref _expr) = do
    checkFailed Unsupported
synthTypeExpr ctx (Application func args) = do
    let argLen = length args
    t <- synthTypeExpr ctx func
    case t of
        TypeFun argTypes retType -> do
            let funArgLen = length argTypes
            when (funArgLen /= argLen) $ checkFailed IncorrectNumberOfArguments
            zipWithM_ (checkTypeExpr ctx) argTypes args
            return retType
        _ -> checkFailed NotAFunction
synthTypeExpr _ctx (TypeApplication _expr _types) = do
    checkFailed Unsupported
synthTypeExpr ctx (DotRecord expr (StellaIdent name)) = do
    t <- synthTypeExpr ctx expr
    case t of
        TypeRecord fields -> getRecordFieldType ctx name fields
        _ -> checkFailed NotARecord
synthTypeExpr ctx (DotTuple expr n) = do
    t <- synthTypeExpr ctx expr
    case t of
        TypeTuple fields -> getTupleFieldType ctx n fields
        _ -> checkFailed NotATuple
synthTypeExpr ctx (Tuple exprs) = do
    types <- mapM (synthTypeExpr ctx) exprs
    return $ TypeTuple types
synthTypeExpr ctx (Record bindings) = do
    types <- mapM
        (\(ABinding name e) -> do
            t <- synthTypeExpr ctx e
            return $ ARecordFieldType name t
        ) bindings
    return $ TypeRecord types
synthTypeExpr _ctx (ConsList _expr1 _expr2) = do
    checkFailed Unsupported  -- TODO: first, urgent
synthTypeExpr _ctx (Head _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
synthTypeExpr _ctx (IsEmpty _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
synthTypeExpr _ctx (Tail _expr) = do
    checkFailed Unsupported  -- TODO: first, urgent
synthTypeExpr _ctx (Panic) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Throw _expr) = do
    checkFailed Unsupported
synthTypeExpr _ctx (TryCatch _exprTry _pattern _exprCatch) = do
    checkFailed Unsupported
synthTypeExpr _ctx (TryWith _expr1 _expr2) = do
    checkFailed Unsupported
synthTypeExpr _ctx (TryCastAs _expr1 _theType _pattern _expr2 _expr3) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Inl _expr) = do
    checkFailed AmbiguousSumType -- FUTURE: subtyping
synthTypeExpr _ctx (Inr _expr) = do
    checkFailed AmbiguousSumType -- FUTURE: subtyping
synthTypeExpr ctx (Succ expr) = do
    checkTypeExpr ctx TypeNat expr
    return TypeNat
synthTypeExpr _ctx (LogicNot _expr) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Pred _expr) = do
    checkFailed Unsupported
synthTypeExpr ctx (IsZero expr) = do
    checkTypeExpr ctx TypeNat expr
    return TypeBool
synthTypeExpr ctx (Fix expr) = do
    eType <- synthTypeExpr ctx expr
    case eType of
        TypeFun lTypes rType -> do
            when (length lTypes /= 1) $ checkFailed IncorrectNumberOfArguments -- ???
            checkTypes ctx rType $ head lType -- ???
            return rType
        _ -> NotAFunction
synthTypeExpr ctx (NatRec count initial step) = do
    checkTypeExpr ctx TypeNat count
    initType <- synthTypeExpr ctx initial
    checkTypeExpr ctx (TypeFun [TypeNat] (TypeFun [initType] initType)) step
    return initType
synthTypeExpr _ctx (Fold _theType _expr) = do
    checkFailed Unsupported
synthTypeExpr _ctx (Unfold _theType _expr) = do
    checkFailed Unsupported
synthTypeExpr _ctx (ConstTrue) = do
    return TypeBool
synthTypeExpr _ctx (ConstFalse) = do
    return TypeBool
synthTypeExpr _ctx (ConstUnit) = do
    return TypeUnit
synthTypeExpr _ctx (ConstInt _n) = do
    return TypeNat
synthTypeExpr _ctx (ConstMemory (MemoryAddress _address)) = do
    checkFailed Unsupported
synthTypeExpr ctx (Var (StellaIdent name)) = do
    getType ctx name

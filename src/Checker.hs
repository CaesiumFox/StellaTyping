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
scanFunctionDecl (DeclFun _ (StellaIdent name) args (NoReturnType) _ _ _) = do
    let aTypes = paramsToTypes args
    mapM_ checkStructureDuplicates aTypes
    return (name, TypeFun aTypes TypeUnit)
scanFunctionDecl (DeclFun _ (StellaIdent name) args (SomeReturnType retType) _ _ _) = do
    let aTypes = paramsToTypes args
    mapM_ checkStructureDuplicates aTypes
    checkStructureDuplicates retType
    return (name, TypeFun aTypes retType)
scanFunctionDecl _ = checkFailed Unsupported

paramsToTypes :: [ParamDecl] -> [Type]
paramsToTypes [] = []
paramsToTypes (AParamDecl _ theType : next) = theType : paramsToTypes next

getFirstMainType :: Context -> CheckResult Type
getFirstMainType ctx = getFirstMainType' $ assocs $ table ctx
  where
    getFirstMainType' [] = checkFailed MissingMain
    getFirstMainType' (("main", t) : _) = return t
    getFirstMainType' (_ : rest) = getFirstMainType' rest

checkExtensionSupport :: Context -> CheckResult ()
checkExtensionSupport ctx = mapM_ checkExtension $ extensions ctx
  where
    checkExtension :: String -> CheckResult ()
    checkExtension _ext = return () -- Simplification
    -- checkExtension "#unit-type"                    = return ()
    -- checkExtension "#pairs"                        = return ()
    -- checkExtension "#tuples"                       = return ()
    -- checkExtension "#records"                      = return ()
    -- checkExtension "#let-bindings"                 = return ()
    -- checkExtension "#type-ascriptions"             = return ()
    -- checkExtension "#sum-types"                    = return ()
    -- checkExtension "#lists"                        = return ()
    -- checkExtension "#variants"                     = return ()
    -- checkExtension "#fixpoint-combinator"          = return ()
    -- checkExtension "#natural-literals"             = return ()
    -- checkExtension "#nullary-functions"            = return ()
    -- checkExtension "#multiparameter-functions"     = return ()
    -- checkExtension _ext = checkFailed Unsupported

{-
checkExtensionEnabled :: Context -> String -> Bool
checkExtensionEnabled ctx extension = checkExtensionEnabledInList (extensions ctx) extension
  where
    checkExtensionEnabledInList [] _ext = False
    checkExtensionEnabledInList (ext' : next) ext
        | ext' == ext = True
        | otherwise   = checkExtensionEnabledInList next ext

checkExtensionEnabledM :: Context -> String -> CheckResult ()
checkExtensionEnabledM ctx extension =
    if checkExtensionEnabled ctx extension
        then return ()
        else checkFailed Unsupported
-}

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
    mainType <- getFirstMainType ctx
    case mainType of
        TypeFun argTypes _ ->
            case argTypes of
                [_] -> return()
                _ -> checkFailed IncorrectArityOfMain
        _ -> checkFailed Unsupported
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

getNameSet :: (Named a) => [a] -> Set String
getNameSet = Data.Set.fromList . map getName

checkNameDuplicates :: (Named a) => CheckErrorCode -> [a] -> CheckResult ()
checkNameDuplicates code named = checkNameDuplicates' code $ map getName named

checkStructureDuplicatesInRecordBinding :: RecordFieldType -> CheckResult ()
checkStructureDuplicatesInRecordBinding (ARecordFieldType _ t) = checkStructureDuplicates t

checkStructureDuplicatesInVariantBinding :: VariantFieldType -> CheckResult ()
checkStructureDuplicatesInVariantBinding (AVariantFieldType _ (SomeTyping t)) = checkStructureDuplicates t
checkStructureDuplicatesInVariantBinding _ = return ()

getVariantTypeByLabel :: [VariantFieldType] -> String -> CheckResult (Maybe Type)
getVariantTypeByLabel [] _label = checkFailed UnexpectedVariantLabel
getVariantTypeByLabel (AVariantFieldType (StellaIdent name) NoTyping : next) label
    | name == label = return Nothing
    | otherwise     = getVariantTypeByLabel next label
getVariantTypeByLabel (AVariantFieldType (StellaIdent name) (SomeTyping t) : next) label
    | name == label = return $ Just t
    | otherwise     = getVariantTypeByLabel next label

checkStructureDuplicates :: Type -> CheckResult ()
checkStructureDuplicates (TypeRecord bindings) = do
    checkNameDuplicates DuplicateRecordTypeFields bindings
    mapM_ checkStructureDuplicatesInRecordBinding bindings
checkStructureDuplicates (TypeVariant bindings) = do
    checkNameDuplicates DuplicateVariantTypeFields bindings
    mapM_ checkStructureDuplicatesInVariantBinding bindings
checkStructureDuplicates (TypeFun aTypes rType) = do
    mapM_ checkStructureDuplicates aTypes
    checkStructureDuplicates rType
checkStructureDuplicates (TypeForAll _ t) = checkStructureDuplicates t
checkStructureDuplicates (TypeRec _ t) = checkStructureDuplicates t
checkStructureDuplicates (TypeSum t1 t2) = do
    checkStructureDuplicates t1
    checkStructureDuplicates t2
checkStructureDuplicates (TypeTuple tt) = mapM_ checkStructureDuplicates tt
checkStructureDuplicates (TypeList t) = checkStructureDuplicates t
checkStructureDuplicates (TypeRef t) = checkStructureDuplicates t
checkStructureDuplicates _ = return ()

checkRecordDuplicates :: [Binding] -> CheckResult ()
checkRecordDuplicates = checkNameDuplicates DuplicateRecordFields

checkRecordInnerTypes :: Context -> [Binding] -> [RecordFieldType] -> CheckResult ()
checkRecordInnerTypes ctx exprs types = do
    let eMap = Data.Map.fromList $ map (\(ABinding (StellaIdent n) e) -> (n, e)) exprs
    mapM_ (
        \(ARecordFieldType (StellaIdent n) t) ->
            case Data.Map.lookup n eMap of
                Just expr -> checkTypeExpr ctx t expr
                Nothing -> checkFailed MissingRecordFields
        )
        types

checkTypeSumCover :: [Pattern] -> CheckResult ()
checkTypeSumCover = checkTypeSumCover' False False
  where
    checkTypeSumCover' :: Bool -> Bool -> [Pattern] -> CheckResult ()
    checkTypeSumCover' hasInl hasInr []
        | hasInl && hasInr = return ()
        | otherwise = checkFailed NonexhaustiveMatchPatterns
    checkTypeSumCover' _hasInl hasInr (PatternInl _ : next) =
        checkTypeSumCover' True hasInr next
    checkTypeSumCover' hasInl _hasInr (PatternInr _ : next) =
        checkTypeSumCover' hasInl True next
    checkTypeSumCover' _ _ _ =
        checkFailed UnexpectedPatternForType

checkTypeVariantCover :: [VariantFieldType] -> [Pattern] -> CheckResult ()
checkTypeVariantCover needed' = checkTypeVariantCover' Data.Set.empty (Data.Set.fromList $ map getName needed')
  where
    checkTypeVariantCover' :: Set String -> Set String -> [Pattern] -> CheckResult ()
    checkTypeVariantCover' covered needed []
        | Data.Set.null (Data.Set.difference needed covered) = return ()
        | otherwise = checkFailed NonexhaustiveMatchPatterns
    checkTypeVariantCover' covered needed (PatternVariant (StellaIdent label) _ : next)
        | Data.Set.notMember label needed = checkFailed UnexpectedPatternForType
        | otherwise = checkTypeVariantCover' (Data.Set.insert label covered) needed next
    checkTypeVariantCover' _ _ _ =
        checkFailed UnexpectedPatternForType

variantToMap :: [VariantFieldType] -> CheckResult (Map String Type)
variantToMap bindings = fmap Data.Map.fromList $ mapM variantFieldToPair bindings
  where
    variantFieldToPair :: VariantFieldType -> CheckResult (String, Type)
    variantFieldToPair (AVariantFieldType _ NoTyping) = checkFailed Unsupported
    variantFieldToPair (AVariantFieldType (StellaIdent name) (SomeTyping t)) = return (name, t)

checkTypeSumCase :: Context -> Type -> Type -> Type -> MatchCase -> CheckResult ()
checkTypeSumCase ctx sample t1 t2 (AMatchCase pat expr) = do
    case pat of
        PatternInl (PatternVar (StellaIdent var)) ->
            checkTypeExpr (assumeType ctx var t1) sample expr
        PatternInr (PatternVar (StellaIdent var)) ->
            checkTypeExpr (assumeType ctx var t2) sample expr
        _ -> checkFailed Unsupported

checkTypeVariantCase :: Context -> Type -> Map String Type -> MatchCase -> CheckResult ()
checkTypeVariantCase ctx sample typeMap (AMatchCase pat expr) = do
    case pat of
        PatternVariant (StellaIdent label) (SomePatternData (PatternVar (StellaIdent var))) ->
            case Data.Map.lookup label typeMap of
                Just t -> checkTypeExpr (assumeType ctx var t) sample expr
                Nothing -> checkFailed Unsupported
        _ -> checkFailed Unsupported

synthTypeSumCase :: Context -> Type -> Type -> MatchCase -> CheckResult Type
synthTypeSumCase ctx t1 t2 (AMatchCase pat expr) = do
    case pat of
        PatternInl (PatternVar (StellaIdent var)) ->
            synthTypeExpr (assumeType ctx var t1) expr
        PatternInr (PatternVar (StellaIdent var)) ->
            synthTypeExpr (assumeType ctx var t2) expr
        _ -> checkFailed Unsupported

synthTypeVariantCase :: Context -> Map String Type -> MatchCase -> CheckResult Type
synthTypeVariantCase ctx typeMap (AMatchCase pat expr) = do
    case pat of
        PatternVariant (StellaIdent label) (SomePatternData (PatternVar (StellaIdent var))) ->
            case Data.Map.lookup label typeMap of
                Just t -> synthTypeExpr (assumeType ctx var t) expr
                Nothing -> checkFailed Unsupported
        _ -> checkFailed Unsupported



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
    checkStructureDuplicates theType
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
            mapM_ checkStructureDuplicates declaredTypes
            zipWithM_ (checkTypesWith ctx UnexpectedTypeForParameter) tParams declaredTypes
            checkTypeExpr (assumeFunctionParams ctx params) tRet expr
        _ -> checkFailed UnexpectedLambda
checkTypeExpr ctx sample (Variant (StellaIdent name) exprData) =
    case exprData of
        NoExprData -> checkFailed Unsupported
        SomeExprData expr ->
            case sample of
                TypeVariant bindings -> do
                    maybeT <- getVariantTypeByLabel bindings name
                    case maybeT of
                        Nothing -> checkFailed Unsupported
                        Just t -> checkTypeExpr ctx t expr
                _ -> checkFailed UnexpectedVariant
checkTypeExpr ctx sample (Match matchedExpr cases) = do
    let patterns = map (\(AMatchCase p _) -> p) cases
    matched <- synthTypeExpr ctx matchedExpr
    case matched of
        TypeSum t1 t2 -> do
            when (length patterns == 0) $ checkFailed IllegalEmptyMatching
            checkTypeSumCover patterns
            mapM_ (checkTypeSumCase ctx sample t1 t2) cases
        TypeVariant labels -> do
            when (length patterns == 0) $ checkFailed IllegalEmptyMatching
            typeMap <- variantToMap labels
            checkTypeVariantCover labels patterns
            mapM_ (checkTypeVariantCase ctx sample typeMap) cases
        _ -> checkFailed Unsupported
checkTypeExpr ctx sample (List exprs) =
    case sample of
        TypeList innerType -> mapM_ (checkTypeExpr ctx innerType) exprs
        _ -> checkFailed UnexpectedList
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
checkTypeExpr ctx sample (Record bindings) = do
    checkRecordDuplicates bindings
    case sample of
        TypeRecord sampleBindings -> do
            --when (length bindings /= length sampleBindings) $ checkFailed UnexpectedTupleLength
            let expectedNameSet = getNameSet sampleBindings
            let actualNameSet = getNameSet bindings
            when (not $ Data.Set.null $ Data.Set.difference expectedNameSet actualNameSet) $ checkFailed MissingRecordFields
            when (not $ Data.Set.null $ Data.Set.difference actualNameSet expectedNameSet) $ checkFailed UnexpectedRecordFields
            checkRecordInnerTypes ctx bindings sampleBindings
        _ -> checkFailed UnexpectedRecord
checkTypeExpr ctx sample (ConsList expr1 expr2) =
    case sample of
        TypeList innerType -> do
            checkTypeExpr ctx innerType expr1
            checkTypeExpr ctx sample expr2
        _ -> checkFailed UnexpectedList
checkTypeExpr ctx sample (Head expr) = do
    checkTypeExpr ctx (TypeList sample) expr
checkTypeExpr ctx sample (IsEmpty expr) = do
    checkTypes ctx sample TypeBool
    t <- synthTypeExpr ctx expr
    case t of
        TypeList _ -> return ()
        _ -> checkFailed NotAList
checkTypeExpr ctx sample (Tail expr) = do
    -- Странная последовательность действий,
    -- но реверс инжиниринг эталонной реализации таков:
    case sample of
        TypeList _sampleInnerType -> do
            checkTypeExpr ctx sample expr
        _ -> do
            argType <- synthTypeExpr ctx expr
            case argType of
                TypeList _ -> checkFailed UnexpectedTypeForExpression
                _ -> checkFailed NotAList
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
checkTypeExpr ctx sample (Inl expr) =
    case sample of
        TypeSum lt _rt -> checkTypeExpr ctx lt expr
        _ -> checkFailed UnexpectedInjection
checkTypeExpr ctx sample (Inr expr) =
    case sample of
        TypeSum _lt rt -> checkTypeExpr ctx rt expr
        _ -> checkFailed UnexpectedInjection
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
        TypeFun lTypes rType ->
            case lTypes of
                [lType] -> do
                    checkTypes ctx sample rType
                    checkTypes ctx rType lType -- ???
                _ -> checkFailed IncorrectNumberOfArguments -- ???
        _ -> checkFailed NotAFunction
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
        else checkFailed UnexpectedTypeForExpression



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
    checkStructureDuplicates theType
    checkTypeExpr ctx theType expr
    return theType
synthTypeExpr _ctx (TypeCast _expr _theType) = do
    checkFailed Unsupported
synthTypeExpr ctx (Abstraction params expr) = do
    --let paramsLen = length params
    let declaredTypes = map (\(AParamDecl _ t) -> t) params
    mapM_ checkStructureDuplicates declaredTypes
    retType <- synthTypeExpr (assumeFunctionParams ctx params) expr
    return $ TypeFun declaredTypes retType
synthTypeExpr _ctx (Variant (StellaIdent _name) _exprData) = do
    checkFailed AmbiguousVariantType
synthTypeExpr ctx (Match matchedExpr cases) = do
    let patterns = map (\(AMatchCase p _) -> p) cases
    matched <- synthTypeExpr ctx matchedExpr
    case matched of
        TypeSum t1 t2 -> do
            when (length patterns == 0) $ checkFailed IllegalEmptyMatching
            checkTypeSumCover patterns
            case cases of
                (first : next) -> do
                    sample <- synthTypeSumCase ctx t1 t2 first
                    mapM_ (checkTypeSumCase ctx sample t1 t2) next
                    return sample
                _ -> checkFailed IllegalEmptyMatching
        TypeVariant labels -> do
            when (length patterns == 0) $ checkFailed IllegalEmptyMatching
            typeMap <- variantToMap labels
            checkTypeVariantCover labels patterns
            case cases of
                (first : next) -> do
                    sample <- synthTypeVariantCase ctx typeMap first
                    mapM_ (checkTypeVariantCase ctx sample typeMap) next
                    return sample
                _ -> checkFailed IllegalEmptyMatching
        _ -> checkFailed Unsupported
synthTypeExpr ctx (List exprs) = do
    case exprs of
        [] -> checkFailed AmbiguousList
        (first : next) -> do
            sample <- synthTypeExpr ctx first
            mapM_ (checkTypeExpr ctx sample) next
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
synthTypeExpr ctx (ConsList expr1 expr2) = do
    sample <- synthTypeExpr ctx expr1
    checkTypeExpr ctx (TypeList sample) expr2
synthTypeExpr ctx (Head expr) = do
    t <- synthTypeExpr ctx expr
    case t of
        TypeList innerType -> return innerType
        _ -> checkFailed NotAList
synthTypeExpr ctx (IsEmpty expr) = do
    t <- synthTypeExpr ctx expr
    case t of
        TypeList _ -> return TypeBool
        _ -> checkFailed NotAList
synthTypeExpr ctx (Tail expr) = do
    t <- synthTypeExpr ctx expr
    case t of
        TypeList _ -> return t
        _ -> checkFailed NotAList
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
    checkFailed AmbiguousSumType
synthTypeExpr _ctx (Inr _expr) = do
    checkFailed AmbiguousSumType
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
            case lTypes of
                [lType] -> do
                    checkTypes ctx rType lType
                    return rType
                _ -> checkFailed IncorrectNumberOfArguments -- ???
        _ -> checkFailed NotAFunction
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

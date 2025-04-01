module CheckError(
    CheckErrorCode(..),
    CheckError(..),
    CheckResult,
    checkFailed
) where

{-
1:

ERROR_MISSING_MAIN
ERROR_UNDEFINED_VARIABLE
ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION
ERROR_NOT_A_FUNCTION
ERROR_NOT_A_TUPLE
ERROR_NOT_A_RECORD
ERROR_NOT_A_LIST
ERROR_UNEXPECTED_LAMBDA
ERROR_UNEXPECTED_TYPE_FOR_PARAMETER
ERROR_UNEXPECTED_TUPLE
ERROR_UNEXPECTED_RECORD
ERROR_UNEXPECTED_VARIANT
ERROR_UNEXPECTED_LIST
ERROR_UNEXPECTED_INJECTION
ERROR_MISSING_RECORD_FIELDS
ERROR_UNEXPECTED_RECORD_FIELDS
ERROR_UNEXPECTED_FIELD_ACCESS
ERROR_UNEXPECTED_VARIANT_LABEL
ERROR_TUPLE_INDEX_OUT_OF_BOUNDS
ERROR_UNEXPECTED_TUPLE_LENGTH
ERROR_AMBIGUOUS_SUM_TYPE
ERROR_AMBIGUOUS_VARIANT_TYPE
ERROR_AMBIGUOUS_LIST
ERROR_ILLEGAL_EMPTY_MATCHING
ERROR_NONEXHAUSTIVE_MATCH_PATTERNS
ERROR_UNEXPECTED_PATTERN_FOR_TYPE
ERROR_DUPLICATE_RECORD_FIELDS
ERROR_DUPLICATE_RECORD_TYPE_FIELDS
ERROR_DUPLICATE_VARIANT_TYPE_FIELDS

ERROR_INCORRECT_ARITY_OF_MAIN
ERROR_INCORRECT_NUMBER_OF_ARGUMENTS
ERROR_UNEXPECTED_NUMBER_OF_PARAMETERS_IN_LAMBDA

ERROR_DUPLICATE_RECORD_PATTERN_FIELDS

ERROR_UNEXPECTED_DATA_FOR_NULLARY_LABEL
ERROR_MISSING_DATA_FOR_LABEL
ERROR_UNEXPECTED_NON_NULLARY_VARIANT_PATTERN
ERROR_UNEXPECTED_NULLARY_VARIANT_PATTERN

2:

ERROR_EXCEPTION_TYPE_NOT_DECLARED
ERROR_AMBIGUOUS_THROW_TYPE
ERROR_AMBIGUOUS_REFERENCE_TYPE
ERROR_AMBIGUOUS_PANIC_TYPE
ERROR_NOT_A_REFERENCE
ERROR_UNEXPECTED_MEMORY_ADDRESS
ERROR_UNEXPECTED_SUBTYPE
-}

data CheckErrorCode
    = Unsupported
    | MissingMain
    | UndefinedVariable
    | UnexpectedTypeForExpression
    | NotAFunction
    | NotATuple
    | NotARecord
    | NotAList
    | UnexpectedLambda
    | UnexpectedTypeForParameter
    | UnexpectedTuple
    | UnexpectedRecord
    | UnexpectedVariant
    | UnexpectedList
    | UnexpectedInjection
    | MissingRecordFields
    | UnexpectedRecordFields
    | UnexpectedFieldAccess
    | UnexpectedVariantLabel
    | TupleIndexOutOfBounds
    | UnexpectedTupleLength
    | AmbiguousSumType
    | AmbiguousVariantType
    | AmbiguousList
    | IllegalEmptyMatching
    | NonexhaustiveMatchPatterns
    | UnexpectedPatternForType
    | DuplicateRecordFields
    | DuplicateRecordTypeFields
    | DuplicateVariantTypeFields
    | IncorrectArityOfMain
    | IncorrectNumberOfArguments
    | UnexpectedNumberOfParametersInLambda
    | DuplicateRecordPatternFields
    | UnexpectedDataForNullaryLabel
    | MissingDataForLabel
    | UnexpectedNonNullaryVariantPattern
    | UnexpectedNullaryVariantPattern
    | ExceptionTypeNotDeclared
    | AmbiguousThrowType
    | AmbiguousReferenceType
    | AmbiguousPanicType
    | NotAReference
    | UnexpectedMemoryAddress
    | UnexpectedSubtype
    deriving(Eq, Ord, Enum);

instance Show CheckErrorCode where
    show Unsupported = "ERROR_UNSUPPORTED"
    show MissingMain = "ERROR_MISSING_MAIN"
    show UndefinedVariable = "ERROR_UNDEFINED_VARIABLE"
    show UnexpectedTypeForExpression = "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION"
    show NotAFunction = "ERROR_NOT_A_FUNCTION"
    show NotATuple = "ERROR_NOT_A_TUPLE"
    show NotARecord = "ERROR_NOT_A_RECORD"
    show NotAList = "ERROR_NOT_A_LIST"
    show UnexpectedLambda = "ERROR_UNEXPECTED_LAMBDA"
    show UnexpectedTypeForParameter = "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER"
    show UnexpectedTuple = "ERROR_UNEXPECTED_TUPLE"
    show UnexpectedRecord = "ERROR_UNEXPECTED_RECORD"
    show UnexpectedVariant = "ERROR_UNEXPECTED_VARIANT"
    show UnexpectedList = "ERROR_UNEXPECTED_LIST"
    show UnexpectedInjection = "ERROR_UNEXPECTED_INJECTION"
    show MissingRecordFields = "ERROR_MISSING_RECORD_FIELDS"
    show UnexpectedRecordFields = "ERROR_UNEXPECTED_RECORD_FIELDS"
    show UnexpectedFieldAccess = "ERROR_UNEXPECTED_FIELD_ACCESS"
    show UnexpectedVariantLabel = "ERROR_UNEXPECTED_VARIANT_LABEL"
    show TupleIndexOutOfBounds = "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS"
    show UnexpectedTupleLength = "ERROR_UNEXPECTED_TUPLE_LENGTH"
    show AmbiguousSumType = "ERROR_AMBIGUOUS_SUM_TYPE"
    show AmbiguousVariantType = "ERROR_AMBIGUOUS_VARIANT_TYPE"
    show AmbiguousList = "ERROR_AMBIGUOUS_LIST"
    show IllegalEmptyMatching = "ERROR_ILLEGAL_EMPTY_MATCHING"
    show NonexhaustiveMatchPatterns = "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS"
    show UnexpectedPatternForType = "ERROR_UNEXPECTED_PATTERN_FOR_TYPE"
    show DuplicateRecordFields = "ERROR_DUPLICATE_RECORD_FIELDS"
    show DuplicateRecordTypeFields = "ERROR_DUPLICATE_RECORD_TYPE_FIELDS"
    show DuplicateVariantTypeFields = "ERROR_DUPLICATE_VARIANT_TYPE_FIELDS"
    show IncorrectArityOfMain = "ERROR_INCORRECT_ARITY_OF_MAIN"
    show IncorrectNumberOfArguments = "ERROR_INCORRECT_NUMBER_OF_ARGUMENTS"
    show UnexpectedNumberOfParametersInLambda = "ERROR_UNEXPECTED_NUMBER_OF_PARAMETERS_IN_LAMBDA"
    show DuplicateRecordPatternFields = "ERROR_DUPLICATE_RECORD_PATTERN_FIELDS"
    show UnexpectedDataForNullaryLabel = "ERROR_UNEXPECTED_DATA_FOR_NULLARY_LABEL"
    show MissingDataForLabel = "ERROR_MISSING_DATA_FOR_LABEL"
    show UnexpectedNonNullaryVariantPattern = "ERROR_UNEXPECTED_NON_NULLARY_VARIANT_PATTERN"
    show UnexpectedNullaryVariantPattern = "ERROR_UNEXPECTED_NULLARY_VARIANT_PATTERN"
    show ExceptionTypeNotDeclared = "ERROR_EXCEPTION_TYPE_NOT_DECLARED"
    show AmbiguousThrowType = "ERROR_AMBIGUOUS_THROW_TYPE"
    show AmbiguousReferenceType = "ERROR_AMBIGUOUS_REFERENCE_TYPE"
    show AmbiguousPanicType = "ERROR_AMBIGUOUS_PANIC_TYPE"
    show NotAReference = "ERROR_NOT_A_REFERENCE"
    show UnexpectedMemoryAddress = "ERROR_UNEXPECTED_MEMORY_ADDRESS"
    show UnexpectedSubtype = "ERROR_UNEXPECTED_SUBTYPE"

data CheckError = CheckError CheckErrorCode String deriving(Eq);

instance Show CheckError where
    show (CheckError code msg) = show code ++ ":\n" ++ msg ++ "\n"

type CheckResult a = Either CheckError a;

checkFailed :: CheckErrorCode -> String -> CheckResult a
checkFailed code msg = Left $ CheckError code msg

module CheckError(
    CheckErrorCode(..),
    CheckResult,
    checkFailed
) where

import Stella.Abs;
import FancyType;

data CheckErrorCode
    = Unsupported
    | MissingMain
    | UndefinedVariable String
    | UnexpectedTypeForExpression Type Type
    | NotAFunction
    | NotATuple
    | NotARecord
    | NotAList
    | UnexpectedLambda
    | UnexpectedTypeForParameter Type Type
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
    | OccursCheckInfiniteType
    | NotAGenericFunction
    | IncorrectNumberOfTypeArguments
    | UndefinedTypeVariable
    deriving(Eq, Ord);

instance Show CheckErrorCode where
    show Unsupported = "ERROR_UNSUPPORTED"
    show MissingMain = "ERROR_MISSING_MAIN"
    show (UndefinedVariable name) = "ERROR_UNDEFINED_VARIABLE\n  " ++ name
    show (UnexpectedTypeForExpression t1 t2) =
        "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION\n  Expected: "
            ++ fancyType t1
            ++ "\n  Actual: "
            ++ fancyType t2
    show NotAFunction = "ERROR_NOT_A_FUNCTION"
    show NotATuple = "ERROR_NOT_A_TUPLE"
    show NotARecord = "ERROR_NOT_A_RECORD"
    show NotAList = "ERROR_NOT_A_LIST"
    show UnexpectedLambda = "ERROR_UNEXPECTED_LAMBDA"
    show (UnexpectedTypeForParameter t1 t2) =
        "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER\n  Expected: "
            ++ fancyType t1
            ++ "\n  Actual: "
            ++ fancyType t2
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
    show OccursCheckInfiniteType = "ERROR_OCCURS_CHECK_INFINITE_TYPE"
    show NotAGenericFunction = "ERROR_NOT_A_GENERIC_FUNCTION"
    show IncorrectNumberOfTypeArguments = "ERROR_INCORRECT_NUMBER_OF_TYPE_ARGUMENTS"
    show UndefinedTypeVariable = "ERROR_UNDEFINED_TYPE_VARIABLE"

type CheckResult a = Either CheckErrorCode a;

checkFailed :: CheckErrorCode -> CheckResult a
checkFailed code = Left code

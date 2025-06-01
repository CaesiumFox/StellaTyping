module Named (
    Named,
    getName
) where

import Stella.Abs

class Named a where
    getName :: a -> String

instance Named ParamDecl where
    getName (AParamDecl name _) = getName name

instance Named LabelledPattern where
    getName (ALabelledPattern name _) = getName name

instance Named Binding where
    getName (ABinding name _) = getName name

instance Named VariantFieldType where
    getName (AVariantFieldType name _) = getName name

instance Named RecordFieldType where
    getName (ARecordFieldType name _) = getName name

instance Named StellaIdent where
    getName (StellaIdent name) = name

instance Named ExtensionName where
    getName (ExtensionName name) = name

instance Named MemoryAddress where
    getName (MemoryAddress name) = name

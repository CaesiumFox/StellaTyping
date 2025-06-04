module FancyType (
    fancyType
) where

import Named
import Stella.Abs

fancyType :: Type -> String
fancyType (TypeAuto) = "auto"
fancyType (TypeFun args ret) = "fn(" ++ (commaSep $ map fancyType args) ++ ") -> " ++ fancyType ret
fancyType (TypeForAll ids t) = "forall " ++ (commaSep $ map getName ids) ++ ". " ++ fancyType t
fancyType (TypeRec name t) = "µ " ++ getName name ++ ". " ++ fancyType t
fancyType (TypeSum l r) = "(" ++ fancyType l ++ " + " ++ fancyType r ++ ")"
fancyType (TypeTuple inner) = "{ " ++ (commaSep $ map fancyType inner) ++ " }"
fancyType (TypeRecord inner) = "{ " ++ (commaSep $ map fancyRecordField inner) ++ " }"
fancyType (TypeVariant inner) = "<| " ++ (commaSep $ map fancyVariantField inner) ++ " |>"
fancyType (TypeList t) = "[" ++ fancyType t ++ "]"
fancyType (TypeBool) = "Bool"
fancyType (TypeNat) = "Nat"
fancyType (TypeUnit) = "Unit"
fancyType (TypeTop) = "Top"
fancyType (TypeBottom) = "Bot"
fancyType (TypeRef t) = "&" ++ fancyType t
fancyType (TypeVar name) = getName name

fancyRecordField :: RecordFieldType -> String
fancyRecordField (ARecordFieldType name t) = getName name ++ " : " ++ fancyType t

fancyVariantField :: VariantFieldType -> String
fancyVariantField (AVariantFieldType name NoTyping) = getName name
fancyVariantField (AVariantFieldType name (SomeTyping t)) = getName name ++ " : " ++ fancyType t

commaSep :: [String] -> String
commaSep [] = ""
commaSep [a] = a
commaSep (a : aa) = a ++ ", " ++ commaSep aa

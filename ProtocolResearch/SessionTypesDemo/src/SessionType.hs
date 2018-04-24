module SessionType
    ( Type(..)
    , SessionType(..)
    , isValid
    , isSubType
    , dual
    , isCompatible
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Placeholder for actual types, must be Eq
data Type
    = INT
    | STRING
    | BOOL
        deriving (Show, Eq, Read)

-- Must be Eq
data SessionType
    = Wait -- end?
    | Kill -- end!
    | Send Type SessionType -- ! Type . SessionType
    | Recv Type SessionType -- ? Type . SessionType
    | Choose (Map String SessionType) -- +{String:SessionType}
    | Offer (Map String SessionType) -- &{String:SessionType}
        deriving (Show, Eq, Read)

isValid' :: Map String SessionType -> Bool
isValid' m
    =   Map.valid m -- Check map structure is valid
    &&  not (Map.null m) -- Check map is not empty
    &&  all isValid m -- Check contents are valid
    &&  Map.notMember "" m -- No empty keys

-- Checks a session type is valid
isValid :: SessionType -> Bool
isValid (Choose m) = isValid' m
isValid (Offer m) = isValid' m
isValid (Send _ a) = isValid a
isValid (Recv _ a) = isValid a
isValid _ = True

-- Determines if the first is a subtype of the second
isSubType :: SessionType -> SessionType -> Bool
-- The subtype can choose more options
isSubType (Choose m) (Choose n)     = Map.isSubmapOfBy isSubType n m
-- The subtype offers fewer options
isSubType (Offer m) (Offer n)       = Map.isSubmapOfBy isSubType m n
isSubType (Send t1 a) (Send t2 b)   = t1 == t2 && isSubType a b
isSubType (Recv t1 a) (Recv t2 b)   = t1 == t2 && isSubType a b
isSubType a b                       = a == b

-- Constructs the dual of a SessionType
dual :: SessionType -> SessionType
dual Wait = Kill
dual Kill = Wait
dual (Send t a) = Recv t (dual a)
dual (Recv t a) = Send t (dual a)
dual (Choose m) = Offer (dual <$> m)
dual (Offer m) = Choose (dual <$> m)

-- Determines if two types can communicate
isCompatible :: SessionType -> SessionType -> Bool
isCompatible a b = isSubType (dual a) b && isSubType (dual b) a

module SessionType
    ( Type(..)
    , SessionType(..)
    , isValid
    , (<:)
    , isSubType
    , dual
    , (<=>)
    , isCompatible
    , union
    , smartUnion
    , strictUnion
    -- , recurse -- Infinite types are messy and impractical
    ) where

import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Function (fix)

-- Placeholder for actual types, must be Eq
data Type
    = INT
    | STRING
    | BOOL
        deriving (Show, Eq, Read)

-- Contents of Choose/Offer
type Branches = Map String SessionType

-- Must be Eq
data SessionType
    = Wait -- end?
    | Kill -- end!
    | Send Type SessionType -- ! Type . SessionType
    | Recv Type SessionType -- ? Type . SessionType
    | Choose Branches -- +{String:SessionType}
    | Offer Branches -- &{String:SessionType}
        deriving (Show, Eq, Read)

isValid' :: Branches -> Bool
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

-- Left is subtype of Right
infix 9 <:
(<:) :: SessionType -> SessionType -> Bool
(<:) = isSubType

-- Determines if the first is a subtype of the second
isSubType :: SessionType -> SessionType -> Bool
-- The subtype chooses from fewer options
isSubType (Choose m) (Choose n)     = Map.isSubmapOfBy isSubType m n
-- The subtype offers extra options
isSubType (Offer m) (Offer n)       = Map.isSubmapOfBy (flip isSubType) n m
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

infix 9 <=>
(<=>) :: SessionType -> SessionType -> Bool
(<=>) = isCompatible

-- Determines if two types can communicate
isCompatible :: SessionType -> SessionType -> Bool
isCompatible a b = a <: dual b -- && b <: dual a (These are equivalent)

strictUnion' :: Branches -> Branches -> Maybe Branches
strictUnion' m n
    | null (Map.intersection m n)   = Just $ Map.union m n
    | otherwise                     = Nothing

-- Unions two SessionTypes if possible and unambiguous
strictUnion :: SessionType -> SessionType -> Maybe SessionType
strictUnion (Choose m) (Choose n)   = Choose <$> strictUnion' m n
strictUnion (Offer m) (Offer n)     = Offer <$> strictUnion' m n
strictUnion _ _                     = Nothing

union' :: Branches -> Branches -> Maybe Branches
union' = Merge.mergeA
    (Merge.traverseMissing $ const Just)
    (Merge.traverseMissing $ const Just)
    (Merge.zipWithAMatched $ const union)

-- Unions two SessionTypes if possible, allows ambiguity
union :: SessionType -> SessionType -> Maybe SessionType
union (Choose m) (Choose n) = Choose <$> union' m n
union (Offer m) (Offer n)   = Offer <$> union' m n
union (Send t1 a) (Send t2 b)
    | t1 == t2  = Send t1 <$> union a b
    | otherwise = Nothing
union (Recv t1 a) (Recv t2 b)
    | t1 == t2  = Recv t1 <$> union a b
    | otherwise = Nothing
union a b
    | a == b    = Just a
    | otherwise = Nothing

smartUnion' :: Branches -> Branches -> Maybe Branches
smartUnion' = Merge.mergeA
    (Merge.traverseMissing $ const Just)
    (Merge.traverseMissing $ const Just)
    (Merge.zipWithAMatched $
        \_ a b -> if a == b then Just a else smartUnion a b)

-- Unions two SessionTypes if possible, allows limited ambiguity
smartUnion :: SessionType -> SessionType -> Maybe SessionType
smartUnion (Choose m) (Choose n)    = Choose <$> smartUnion' m n
smartUnion (Offer m) (Offer n)      = Offer <$> smartUnion' m n
smartUnion _ _                      = Nothing

-- Nothing complicated
simpleUnion :: [(String, SessionType)] -> Maybe SessionType
simpleUnion [] = Nothing
simpleUnion list = Just $ Offer (Map.fromList list)

{-  Turns a looping or recursive session type into a fully defined
    (possibly infinite) session type -}
recurse :: (SessionType -> SessionType) -> SessionType
recurse = fix

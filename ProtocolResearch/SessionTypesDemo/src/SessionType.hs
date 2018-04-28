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
    , strictUnion
    ) where

import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

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
(<:) = flip isSubType

-- Determines if the second is a subtype of the first
isSubType :: SessionType -> SessionType -> Bool
-- The subtype can choose more options
isSubType (Choose m) (Choose n)     = Map.isSubmapOfBy (<:) m n
-- The subtype offers fewer options
isSubType (Offer m) (Offer n)       = Map.isSubmapOfBy (<:) n m
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
isCompatible a b = dual a <: b -- && dual b <: a (These are equivalent)

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
    (Merge.traverseMissing $ \_ x -> Just x)
    (Merge.traverseMissing $ \_ x -> Just x)
    (Merge.zipWithAMatched $ \_ x y -> union x y)

-- Unions two SessionTypes if possible and possibly ambiguous
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

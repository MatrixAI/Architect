module SessionType
    ( Type(..)
    , Branches
    , SessionType
    , end, send, recv, offer, choose, mu, ref, join
    , isValid
    , (<:)
    , isSubType
    , dual
    , (<=>)
    , isCompatible
    -- , union
    -- , smartUnion
    -- , strictUnion
    -- , simpleUnion
    ) where

import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Fix (Fix(..), unFix, cata)
import Control.Arrow ((>>>))
import Control.Applicative (liftA2)

-- Zips a Fix type
zipo :: Functor f => (f (Fix f -> a) -> f (Fix f) -> a) -> Fix f -> Fix f -> a
zipo alg = cata (\x -> alg x . unFix)

-- Placeholder for actual types, must be Eq
data Type
    = INT
    | STRING
    | BOOL
        deriving (Show, Eq, Read)

-- Contents of Choose/Offer
type BranchesF a = Map String a
type Branches = BranchesF SessionType

-- Must be Eq
data SessionTypeF a
    = Send Type -- ! Type
    | Recv Type -- ? Type
    | Choose (BranchesF [a]) -- +{String:SessionType}
    | Offer (BranchesF [a]) -- &{String:SessionType}
    | Mu [a] -- Fixpoint
    | Ref Integer -- Reference to Fixpoint
        deriving (Show, Eq, Read)

instance Functor SessionTypeF where
    fmap _ (Send t)     = Send t
    fmap _ (Recv t)     = Recv t
    fmap f (Choose m)   = Choose (fmap (fmap f) m)
    fmap f (Offer m)    = Offer (fmap (fmap f) m)
    fmap f (Mu s)       = Mu (fmap f s)
    fmap _ (Ref n)      = Ref n

type SimpleSession = Fix SessionTypeF
type SessionType = [SimpleSession]

end :: SessionType
end = []

send :: Type -> SessionType
send = pure . Fix . Send

recv :: Type -> SessionType
recv = pure . Fix . Recv

choose :: Branches -> SessionType
choose = pure . Fix . Choose

offer :: Branches -> SessionType
offer = pure . Fix . Offer

mu :: SessionType -> SessionType
mu = pure . Fix . Mu

ref :: Integer -> SessionType
ref = pure . Fix . Ref

join :: SessionType -> SessionType -> SessionType
join = (++)

-- Determine if branches are valid and return largest reference
isValidBranches :: BranchesF [Maybe Integer] -> Maybe Integer
isValidBranches m =
    if      Map.valid m -- Check map structure is valid
        &&  not (Map.null m) -- Check map is not empty
        &&  Map.notMember "" m -- No empty keys
    then
        -- Check contents are valid and return largest reference
        foldl (liftA2 max) (Just 0) (foldl (liftA2 max) (Just 0) <$> m)
    else
        Nothing

-- Determine if a simple session type is valid and return largest reference
isValidSimple :: SimpleSession -> Maybe Integer
isValidSimple = cata $ \x -> case x of
    Choose m    -> isValidBranches m
    Offer m     -> isValidBranches m
    Mu ss       -> pred <$> foldl (liftA2 max) (Just 0) ss
    Ref n       -> if n > 0
                    then Just n
                    else Nothing
    _           -> Just 0

-- Determines if a session type is valid
isValid :: SessionType -> Bool
isValid =
        fmap isValidSimple -- Get validity and largest reference of each
    >>> foldl (liftA2 max) (Just 0) -- Get largest reference of all
    >>> fmap (>0) -- Check if any references are out of bounds
    >>> (== Just False) -- Valid if largest wasn't out of bounds

-- Left is subtype of Right
infix 8 <:
(<:) :: SessionType -> SessionType -> Bool
(<:) = isSubType

-- Checks two lists are the same size, then zips and checks all yield true
checkLists :: (a -> b -> Bool) -> [a] -> [b] -> Bool
checkLists f x y = (length x == length y) && and (zipWith f x y)

-- Determines if the first is a subtype of the second
isSubTypeSimple :: SimpleSession -> SimpleSession -> Bool
isSubTypeSimple = zipo $ \a b -> case (a,b) of
    (Send t, Send u)        -> t == u
    (Recv t, Recv u)        -> t == u
    -- The subtype chooses from fewer options
    (Choose m, Choose n)    -> Map.isSubmapOfBy (checkLists ($)) m n
    -- The subtype offers extra options
    (Offer m, Offer n)      -> Map.isSubmapOfBy (checkLists $ flip ($)) n m
    (Mu a, Mu b)            -> checkLists ($) a b
    (Ref k, Ref l)          -> k == l
    _                       -> False

-- Determines if the first is a subtype of the second
isSubType :: SessionType -> SessionType -> Bool
isSubType = checkLists isSubTypeSimple

-- Constructs the dual of a simple session type
dualSimple :: SimpleSession -> SimpleSession
dualSimple = cata $ \x -> case x of
    Send t      -> Fix $ Recv t
    Recv t      -> Fix $ Send t
    Choose m    -> Fix $ Offer m
    Offer m     -> Fix $ Choose m
    s           -> Fix s

-- Constructs the dual of a SessionType
dual :: SessionType -> SessionType
dual = fmap dualSimple

infix 8 <=>
(<=>) :: SessionType -> SessionType -> Bool
(<=>) = isCompatible

-- Determines if two types can communicate
isCompatible :: SessionType -> SessionType -> Bool
isCompatible a b = a <: dual b -- && b <: dual a (These are equivalent)

-- strictUnion' :: Branches -> Branches -> Maybe Branches
-- strictUnion' m n
--     | null (Map.intersection m n)   = Just $ Map.union m n
--     | otherwise                     = Nothing
--
-- -- Unions two SessionTypes if possible and unambiguous
-- strictUnion :: SessionType -> SessionType -> Maybe SessionType
-- strictUnion (Choose m) (Choose n)   = Choose <$> strictUnion' m n
-- strictUnion (Offer m) (Offer n)     = Offer <$> strictUnion' m n
-- strictUnion _ _                     = Nothing
--
-- union' :: Branches -> Branches -> Maybe Branches
-- union' = Merge.mergeA
--     (Merge.traverseMissing $ const Just)
--     (Merge.traverseMissing $ const Just)
--     (Merge.zipWithAMatched $ const union)
--
-- -- Unions two SessionTypes if possible, allows ambiguity
-- union :: SessionType -> SessionType -> Maybe SessionType
-- union (Choose m) (Choose n) = Choose <$> union' m n
-- union (Offer m) (Offer n)   = Offer <$> union' m n
-- union (Send t1 a) (Send t2 b)
--     | t1 == t2  = Send t1 <$> union a b
--     | otherwise = Nothing
-- union (Recv t1 a) (Recv t2 b)
--     | t1 == t2  = Recv t1 <$> union a b
--     | otherwise = Nothing
-- union a b
--     | a == b    = Just a
--     | otherwise = Nothing
--
-- smartUnion' :: Branches -> Branches -> Maybe Branches
-- smartUnion' = Merge.mergeA
--     (Merge.traverseMissing $ const Just)
--     (Merge.traverseMissing $ const Just)
--     (Merge.zipWithAMatched $
--         \_ a b -> if a == b then Just a else smartUnion a b)
--
-- -- Unions two SessionTypes if possible, allows limited ambiguity
-- smartUnion :: SessionType -> SessionType -> Maybe SessionType
-- smartUnion (Choose m) (Choose n)    = Choose <$> smartUnion' m n
-- smartUnion (Offer m) (Offer n)      = Offer <$> smartUnion' m n
-- smartUnion _ _                      = Nothing
--
-- -- Nothing complicated
-- simpleUnion :: [(String, SessionType)] -> Maybe SessionType
-- simpleUnion [] = Nothing
-- simpleUnion list = Just $ Offer (Map.fromList list)

{-# LANGUAGE LambdaCase    #-}

import Prelude hiding (fst, snd, curry, succ)
import qualified Prelude

type Hom a b = a -> b

data Nat = Zero | Succ Nat

idT :: Hom a a
idT = id

-- this is functional composition
-- this is straight morphism composition
compose :: Hom a b -> Hom b c -> Hom a c
compose = flip (.)

unit :: Hom a ()
unit = const ()

pair :: Hom a b -> Hom a c -> Hom a (b, c)
pair f g a = (f a, g a)

fst :: Hom (a, b) a
fst = Prelude.fst

snd :: Hom (a, b) b
snd = Prelude.snd

curry :: Hom (a, b) c -> Hom a (b -> c)
curry = Prelude.curry

eval :: Hom ((a -> b), a) b
eval (f, v) = f v

zero :: Hom () Nat
zero () = Zero

succ :: Hom Nat Nat
succ n = Succ n

iter :: Hom a b -> Hom (a, b) b -> Hom (a, Nat) b
iter base recurse (a, n) = case n of
  Zero   -> base a
  Succ n -> recurse (a, iter base recurse (a, n))

-- this language only has the primitive value of natural numbers
-- here we use this to run through some closed system T expression
-- it has to be closed in the sense that it takes no further parameters
-- that's why we assume it takes ()
run :: Hom () Nat -> Int
run e = loop $ e ()
 where
  loop = \case
    Zero   -> 0
    Succ n -> 1 + loop n

result = run $ zero `compose` succ

-- this is actually not meant to work
-- but this type checks because of Haskell's laziness
-- note that bad is a Hom () Nat
bad :: Hom () Nat
bad = compose bad succ

-- System T language
-- will be compiled to the combinator form above

type TVar = String
data TType = One | Prod TType TType | Arrow TType TType | Nat
data TTerm = Var TVar
           | Let TVar TTerm TTerm
           | Lam TVar TTerm
           | App TTerm TTerm
           | Unit
           | Pair TTerm TTerm
           | Fst TTerm
           | Snd TTerm
           | ZeroT
           | SuccT TTerm
           | Iter TTerm TTerm TVar TTerm
           | Annot TTerm TType

-- can be part of the show type class
-- notice how the arrow form is matched twice, this allows us to change our pretty printer!
showTType :: TType -> String
showTType ttype = case ttype of
  One -> "unit"
  Nat -> "nat"
  Prod ttype1 ttype2 ->
    "(" ++ showTType ttype1 ++ " * " ++ showTType ttype2 ++ ")"
  Arrow ttype1@(Arrow _ _) ttype2 ->
    "(" ++ showTType ttype1 ++ ") -> " ++ showTType ttype2
  Arrow ttype1 ttype2 -> showTType ttype1 ++ " -> " ++ showTType ttype2


-- a type checker here is a elaborater
-- this is a common pattern in type based compilation
-- we don't just type chek a term, we also produce a term in the output language
-- as part of the type checking process
-- furthermore we use bidirectional typechecking
-- we split the typechecker into 2 functions
-- check and synth
-- the check function takes a term and the type check it against it and returns the elaborated code
-- the synth function takes a teram and infers a tyep for that term in addition to emitting the code for that term
-- note that Context is a monad

-- we don't really have AST.Expr
-- since we are not using an intermediate macro system
-- we are just using our ADT to represent the object language
-- and we have a combinatory language above
-- this is useful to understand bidirectional type checking here

check :: TTerm -> TType -> Context TTerm
check tterm ttype = undefined

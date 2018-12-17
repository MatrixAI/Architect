{-# LANGUAGE GADTs #-}

module SystemTCombinator where

-- untyped version
data THom = Id
          | Unit
          | Zero
          | Succ
          | Compose THom THom
          | Pair THom THom
          | Fst
          | Snd
          | Curry THom
          | Eval
          | Iter THom THom

-- SystemT is a finite type extension of primitive recursive arithmetic
-- it was developed by Kurt Godel to provide a consistency proof of arithmetic
-- it ends up being a sort of small typed functional language
-- The main novelty with respect to simple type theory is the fact that the theory contains “rules which permit the definition of a function by an equality with a term constructed from variables and previously defined functions or by primitive recursion with respect to a number variable”. Thus, unlike simple type theory that permits only the construction of explicitly definable functions, the system T also permits the construction of functions by induction.
-- for the background on this language and why it's important to typed functional programming, see: http://www.lsv.fr/~dowek/Philo/godel.pdf
-- this language is total, in that it will always terminate
-- in the same way that Godel's theory can be seen as an extension to simple type theory, then this language is also an extension of simply typed combinator language extended with the construction of functions by induction
-- SystemT is arguably the first proper total functional programming language published in 1958
-- Godel extended the simply-typed lambda calculus with a type of natural numbers, and iteration over them (logicians call this system "higher-type arithmetic")
-- it's a combinatory language though, there's no actual lambda calculus as there is no function abstraction, nor variable binders

-- data TNat = Zero | Succ TNat

-- data THom a b where
--   Id :: THom a a
--   Unit :: THom a () -- Unique map ! (A -> 1)
--   ZeroH :: THom () TNat
--   SuccH :: THom TNat TNat
--   Compose :: THom a b -> THom b c -> THom a c -- diagramatic composition ;
--   Pair :: THom a b -> THom a c -> THom a (b, c) -- f : A -> X, A -> Y, then A -> X*Y
--   Fst :: THom (a, b) a
--   Snd :: THom (a, b) b
--   Curry :: THom (a, b) c -> THom a (b -> c)
--   Eval :: THom ((a -> b), a) b -- (A = B) * A -> B
--   Iter :: THom a b -> THom (a, b) b -> THom (a, TNat) b

-- the above GADT describes the language, however later in order to interpret the language
-- the interpreter below interprets THom as a function in Haskell
-- and gives implementation for each combinator
-- to give an implementation is to also intepret these things
-- so the above is a free monad, and then we have an interpreter of these things
-- I'm commenting them out for now until I figure out how to write the interpreter

-- import Prelude hiding (fst, snd, curry, succ)
-- import qualified Prelude
-- type Hom a b = a -> b
-- data Nat = Zero | Succ Nat
-- idT :: Hom a a
-- idT = id
-- -- this is not functional composition
-- -- this is straight morphism composition
-- compose :: Hom a b -> Hom b c -> Hom a c
-- compose = flip (.)
-- unit :: Hom a ()
-- unit = const ()
-- pair :: Hom a b -> Hom a c -> Hom a (b, c)
-- pair f g a = (f a, g a)
-- fst :: Hom (a, b) a
-- fst = Prelude.fst
-- snd :: Hom (a, b) b
-- snd = Prelude.snd
-- curry :: Hom (a, b) c -> Hom a (b -> c)
-- curry = Prelude.curry
-- eval :: Hom ((a -> b), a) b
-- eval (f, v) = f v
-- zero :: Hom () Nat
-- zero () = Zero
-- succ :: Hom Nat Nat
-- succ n = Succ n
-- iter :: Hom a b -> Hom (a, b) b -> Hom (a, Nat) b
-- iter base recurse (a, n) = case n of
--   Zero   -> base a
--   Succ n -> recurse (a, iter base recurse (a, n))
-- -- this language only has the primitive value of natural numbers
-- -- here we use this to run through some closed system T expression
-- -- it has to be closed in the sense that it takes no further parameters
-- -- that's why we assume it takes ()
-- run :: Hom () Nat -> Int
-- run e = loop $ e ()
--  where
--   loop = \case
--     Zero   -> 0
--     Succ n -> 1 + loop n

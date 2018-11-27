{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SystemTLambda where

import           qualified SystemTCombinator as SystemTC
import Control.Monad.Except

-- so SystemTC now just exposes THom type (..)
-- constructors that we need to work here


type TVar = String
data TType = One | Prod TType TType | Arrow TType TType | Nat deriving (Eq)
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

type Context = [(TVar, TType)]
newtype ReaderError a = ReaderError { run :: Context -> Either String a }

instance Functor ReaderError where
  fmap f xs = ReaderError $ \ctx ->
    either Left (\v -> Right $ f v) $ run xs ctx

instance Applicative ReaderError where
  pure a = ReaderError $ \ctx -> Right a
  fs <*> xs = ReaderError $ \ctx ->
    either Left (\f -> fmap f $ run xs ctx) (run fs ctx)

instance Monad ReaderError where
  xs >>= f = ReaderError $ \ctx ->
    either Left (\v -> run (f v) ctx) $ run xs ctx

instance MonadError String ReaderError where
  throwError e = ReaderError $ const $ Left e
  catchError xs f = ReaderError $ \ctx ->
    either (\e -> run (f e) ctx) Right $ run xs ctx

-- a hypothesis is a (TVar, TType)
-- it is 1 judgement within the context
-- we take a ReaderError a, and return ReaderError a
-- with a new hypothesis in its context!
withHyp :: (TVar, TType) -> ReaderError a -> ReaderError a
withHyp hyp cmd = ReaderError $ \ctx -> run cmd (hyp : ctx)

-- it seems that fst/snd may be used to help fidnign the right type
-- it's part of the lookup function
-- which does what...?
-- it looks up a variable inside a context
-- and if it finds it, it returns Right ...
-- its a ReaderError thing
-- and if it doesn't find it... it returns an error
-- i really don't think weneed ot use an infinite type to do this
-- it's only used there... lol


-- find receives a context and constructs the appropriate projection
-- SystemTC.Compose SystemTC.Fst (SystemTC.Compose SystemTC.Fst SystemTC.Snd)
-- THom (((a, c), b1), b2) c

-- the problem is that find is actually returning an infinite type
-- the Snd and Fst composition can return more larger and larger types
-- it seems we could use Fix types to deal with this, to actually get us a recursive potentially infinite type
-- but it results it much more complexity for this function
-- the reason the find works in that case, is because it is masked
-- the whole thing gets put inside an AST node
-- so really we are dealing with a seris of projectsions
-- i'm still not entirely sure what this is needed for

-- it seems we need to existentially quantify our THom types
-- otherwise there doesn't seem to be a way to produce what we want
-- data TExpr = forall a b. TExpr (SystemTC.THom a b)

-- find :: TVar -> Context -> _
-- find tvar [] = error "Not Found"
-- find tvar ((tvar', ttype):ctx)
--   | tvar == tvar' = SystemTC.Snd
--   | otherwise     = SystemTC.Compose SystemTC.Fst (find tvar ctx)

-- it's a maybe
-- if we find it
-- the problem is that the "term"
-- is an infinite type atm
-- the gadt Compose is meant to give back THom a c
-- but the a gets expanded by the Fst and Snd
-- so we end up with something strange
-- we need to wrap this type some how
-- becuase I'm pretty sure you still need the types there
-- THom a b is still important right
-- you wouldn't want to have no types there at all
-- lookupTvar :: TVar -> ReaderError (THom _ _, TType)
-- lookupTvar tvar = ReaderError $ \ctx ->
--   case lookup tvar ctx of
--     Just ttype -> (_, ttype)
--     Nothing -> Left "tvar is unbound"

--

-- because it can be a projection expression
-- that is so weird

-- this seems to try to find a x in the a context
-- if it finds the x, it returns a snd expression
-- otherwise it will compose fst with find x ctx
-- why the hell is doing this?
-- and we would be returning THom
-- not Goel

-- this an exception system?
-- for some reason

-- check :: TTerm -> TType -> Context THom
-- check tterm ttype = undefined
-- check :: TTerm -> TType -> Context TTerm
-- check tterm ttype = case (tterm ttype) of
--   (Lam x e, Arrow ttype1 ttype2) -> undefined
--   (Lam _ _, ttype) -> error $ "Unexpected function type " ++ showTType ttype
--   (Unit, One) -> return unit


-- so our Hom thing no longer has those type variables
-- our type checker will need ot run against untyped output and
-- have runtime probelms
-- but here we should check the types already
-- a reader error needs to take a context

-- we don't need a context if we are just a unit

check :: TTerm -> TType -> ReaderError SystemTC.THom
check (Let tvar tterm1 tterm2) ttype = undefined
check (Lam tvar tterm) (Arrow ttype1 ttype2) = undefined
check (Lam _ _) ttype = throwError
  ("expected function type, got '" ++ showTType ttype ++ "'")
check Unit One = ReaderError $ const $ Right SystemTC.UnitH
check Unit ttype = throwError
  ("expected unit type, got '" ++ showTType ttype ++ "'")
check (Pair tterm1 tterm2) (Prod ttype1 ttype2) = undefined
check (Pair _ _) ttype = throwError
  ("expected product type, got '" ++ showTType ttype ++ "'")
check (Iter baseTerm inductTerm tvar numTerm) ttype = do
  baseHom   <- check baseTerm ttype
  inductHom <- withHyp (tvar, ttype) (check inductTerm ttype)
  numHom    <- check numTerm Nat
  return $ SystemTC.ComposeH
    (SystemTC.PairH SystemTC.IdH numHom)
    (SystemTC.IterH baseHom inductHom)
check tterm ttype = do
  (thom, ttype') <- synth tterm
  if ttype == ttype'
  then return thom
  else throwError $
    "expected type '" ++
    showTType ttype ++
    "', inferred type '" ++
    showTType ttype' ++
    "'"



synth :: TTerm -> ReaderError (SystemTC.THom, TType)
synth = undefined

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module SystemTLambda where

import           Control.Monad.Catch
import qualified SystemTCombinator    as SystemTC

type TVar = String

data TType = One | Prod TType TType | Arrow TType TType | Nat deriving (Eq)

instance Show TType where
  show ttype = case ttype of
    One -> "unit"
    Nat -> "nat"
    Prod ttype1 ttype2 ->
      "(" ++ show ttype1 ++ " * " ++ show ttype2 ++ ")"
    Arrow ttype1@(Arrow _ _) ttype2 ->
      "(" ++ show ttype1 ++ ") -> " ++ show ttype2
    Arrow ttype1 ttype2 -> show ttype1 ++ " -> " ++ show ttype2

data TTerm = Var TVar
           | Let TVar TTerm TTerm
           | Lam TVar TTerm
           | App TTerm TTerm
           | Unit
           | Pair TTerm TTerm
           | Fst TTerm
           | Snd TTerm
           | Zero
           | Succ TTerm
           | Iter TTerm TTerm TVar TTerm
           | Annot TTerm TType
           deriving (Show)

-- a context is a list of hypotheses/judgements
type TContext = [(TVar, TType)]

-- our exceptions for SystemT
data TException = TypeCheckException String
                | BindingException String
  deriving (Show)

instance Exception TException

newtype Parser a = Parser { run :: TContext -> Either SomeException a }

instance Functor Parser where
  fmap f xs = Parser $ \ctx ->
    either Left (\v -> Right $ f v) $ run xs ctx

instance Applicative Parser where
  pure a = Parser $ \ctx -> Right a
  fs <*> xs = Parser $ \ctx ->
    either Left (\f -> fmap f $ run xs ctx) (run fs ctx)

instance Monad Parser where
  xs >>= f = Parser $ \ctx ->
    either Left (\v -> run (f v) ctx) $ run xs ctx

instance MonadThrow Parser where
  throwM e = Parser (const $ Left $ toException e)

instance MonadCatch Parser where
  catch p f = Parser $ \ctx ->
    either
      (\e -> case fromException e of
        Just e' -> run (f e') ctx -- this handles the exception
        Nothing -> Left e) -- this propagates it upwards
      Right
      $ run p ctx

-- try this!
-- throwM (TypeCheckException "hah") `catch` \e -> putStrLn ("Caught " ++ show (e :: TException))

withHypothesis :: (TVar, TType) -> Parser a -> Parser a
withHypothesis hyp cmd = Parser $ \ctx -> run cmd (hyp : ctx)

-- this is not productive
-- at most it will iterate over the entire list
-- before returning a result
-- foldr version!
tvarToHom :: TVar -> Parser (SystemTC.THom, TType)
tvarToHom var = Parser $ \ctx ->
  case foldr transform Nothing ctx of
    Just x -> Right x
    Nothing -> throwM $ BindingException ("unbound variable " ++ show var)
  where
    transform (var', varType) homAndType =
      if var == var'
      then Just (SystemTC.Snd, varType)
      else homAndType >>= (\(varHom, varType) -> Just (SystemTC.Compose SystemTC.Fst varHom, varType))

-- tvarToHom :: TVar -> Parser (SystemTC.THom, TType)
-- tvarToHom tvar = Parser $ \ctx ->
--   case go tvar ctx of
--     Just x -> Right x
--     Nothing -> throwM $ BindingException ("unbound variable " ++ show tvar)
--   where
--     -- this is fold operation!
--     -- we are folding a context list into a Maybe of (Thom TType)
--     go :: TVar -> TContext -> Maybe (SystemTC.THom, TType)
--     go var [] = Nothing
--     go var ((var', varType):ctx) | var == var' = Just (SystemTC.Snd, varType)
--                                  | otherwise   = do
--                                  (varHom, varType) <- go var ctx
--                                  return (SystemTC.Compose SystemTC.Fst varHom, varType)

-- ok so this is the bidirection type checking

check :: TTerm -> TType -> Parser SystemTC.THom
-- check a lambda
check (Lam var bodyTerm) (Arrow varType bodyType) =
  withHypothesis (var, varType) $
  check bodyTerm bodyType >>= (\bodyHom -> return $ SystemTC.Curry bodyHom)
check (Lam _    _    ) ttype                 = throwM
  $ TypeCheckException ("expected function type, got '" ++ show ttype ++ "'")
-- check unit
check Unit One = return SystemTC.Unit
check Unit ttype =
  throwM $ TypeCheckException ("expected unit type, got '" ++ show ttype ++ "'")
-- check products
check (Pair term1 term2) (Prod ttype1 ttype2) = do
  hom1 <- check term1 ttype1
  hom2 <- check term2 ttype2
  return $ SystemTC.Pair hom1 hom2
check (Pair _      _     ) ttype                = throwM
  $ TypeCheckException ("expected product type, got '" ++ show ttype ++ "'")
-- check primitive recursion
check (Iter baseTerm inductTerm tvar numTerm) ttype = do
  baseHom   <- check baseTerm ttype
  inductHom <- withHypothesis (tvar, ttype) (check inductTerm ttype)
  numHom    <- check numTerm Nat
  return $ SystemTC.Compose (SystemTC.Pair SystemTC.Id numHom)
                            (SystemTC.Iter baseHom inductHom)
-- check let bindings
check (Let var valueTerm exprTerm) exprType = do
  (valueHom, valueType) <- synth valueTerm
  exprHom <- withHypothesis (var, valueType) (check exprTerm exprType)
  return $ SystemTC.Compose (SystemTC.Pair SystemTC.Id valueHom) exprHom
check tterm ttype = do
  (thom, ttype') <- synth tterm
  if ttype == ttype'
    then return thom
    else throwM $ TypeCheckException
      (  "expected type '"
      ++ show ttype
      ++ "', inferred type '"
      ++ show ttype'
      ++ "'"
      )

-- why synth Let if you don't need it
-- also synth is infer plus conversion to Hom
-- since check is also used wiht let
-- because the check calls synth instead of check again
-- so synth has to potentially take a Let binding as well
synth :: TTerm -> Parser (SystemTC.THom, TType)
synth (Var tvar) = tvarToHom tvar
synth (Let var valueTerm exprTerm) = do
  (valueHom, valueType) <- synth valueTerm
  (exprHom, exprType) <- withHypothesis (var, valueType) (synth exprTerm)
  return (SystemTC.Compose (SystemTC.Pair SystemTC.Id valueHom) exprHom, exprType)
synth (App functionTerm valueTerm) = do
  (functionHom, functionType) <- synth functionTerm
  case functionType of
    Arrow headType bodyType -> do
      valueHom <- check valueTerm headType
      return (SystemTC.Compose (SystemTC.Pair functionHom valueHom) SystemTC.Eval, bodyType)
    _ -> throwM $ TypeCheckException ("expected function, got '" ++ show functionType ++ "'")
synth (Fst pairTerm) = do
  (pairHom, pairType) <- synth pairTerm
  case pairType of
    Prod fstType sndType -> return (SystemTC.Compose pairHom SystemTC.Fst, fstType)
    _ -> throwM $ TypeCheckException ("expected product, got '" ++ show pairType ++ "'")
synth (Snd pairTerm) = do
  (pairHom, pairType) <- synth pairTerm
  case pairType of
    Prod fstType sndType -> return (SystemTC.Compose pairHom SystemTC.Snd, sndType)
    _ -> throwM $ TypeCheckException ("expected product, got '" ++ show pairType ++ "'")
synth Zero = return (SystemTC.Compose SystemTC.Unit SystemTC.Zero, Nat)
synth (Succ numTerm) = do
  numHom <- check numTerm Nat
  return (SystemTC.Compose numHom SystemTC.Succ, Nat)
synth (Annot term ttype) = do
  hom <- check term ttype
  return (hom, ttype)
synth _ = throwM $ TypeCheckException "unknown synthesis"

-- WOOOHOO!

result :: TTerm
result =
  Let "sum"
    (Annot
      (Lam "x" $ Lam "y" $
       Iter (Var "y") (Succ $ Var "n") "n" (Var "x"))
      (Arrow Nat $ Arrow Nat Nat))
    (App
      (App
        (Var "sum")
        (Succ $ Succ Zero))
      (Succ $ Succ $ Succ Zero))

result2 = run (synth result) []

-- do we use check or synth as the entrypoint?
-- it seems to mean that you always use synth as an entrypoint
-- since you start always with terms that you need to synthesize 
-- but also if you know that your program can be IO ()
-- then you would always enter with check, that the main is IO ()
-- but that doesn't really work either...

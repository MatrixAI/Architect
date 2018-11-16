module SystemTLambda where

import           SystemTCombinator

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

-- check :: TTerm -> TType -> Context THom
-- check tterm ttype = undefined
-- check :: TTerm -> TType -> Context TTerm
-- check tterm ttype = case (tterm ttype) of
--   (Lam x e, Arrow ttype1 ttype2) -> undefined
--   (Lam _ _, ttype) -> error $ "Unexpected function type " ++ showTType ttype
--   (Unit, One) -> return unit

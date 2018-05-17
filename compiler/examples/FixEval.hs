{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}

import Data.Text
import Data.Fix (Fix (..), cata, ana)


-- NConstant !NAtom
-- NStr (!NString r)
--

data Antiquoted (v :: *) (r :: *) = Plain v
                                  | EscapedNewline
                                  | Antiquoted r
                                  deriving (Functor)

-- atoms evaluate to themselves
-- they appear in both the parsed AST and evaluated Form
-- based on Atom.hs
data AAtom = AInt Integer
           | AFloat Float
           | ABool Bool
           | ANull
           deriving (Eq, Ord, Show)

-- strict strings!?
data AString r = DoubleQuoted ![Antiquoted Text r]
               | Indented !Int ![Antiquoted Text r]
               deriving (Functor)

-- data AExprF r = AConstant AAtom
--               | AString (AString r)
--               deriving

-- type AExpr = Fix AExprF

-- aEval :: AExprF Int -> Int
-- aEval (AConstant i) = i
-- aEval (x `Add` y) = x + y
-- aEval (x `Mul` y) = x * y

-- eval :: AExprF AExpr -> Int
-- eval e = cata f $ Fix e
--   where
--     f (AConstant i) = i
--     f (x `Add` y) = x + y
--     f (x `Mul` y) = x * y

-- result :: Int
-- result = eval (AConstant 1)

-- -- the idea is that
-- -- we can use things to evaluate stuff
-- -- so one way is to evaluate this
-- -- our evaluator just doesn't even use the fmap
-- -- catamorphism makes use of the functor!
-- -- I see

-- so there is a evaluator


-- | This is the entry point for all evaluations, whatever the expression tree
--   type. It sets up the common Nix environment and applies the
--   transformations, allowing them to be easily composed.
nixEval :: (MonadNix e m, Has e Options, Functor f)
        => Maybe FilePath -> Transform f (m a) -> Alg f (m a) -> Fix f -> m a
nixEval mpath xform alg = withNixContext mpath . adi alg xform

-- | Evaluate a nix expression in the default context
nixEvalExpr :: forall e m. (MonadNix e m, Has e Options)
            => Maybe FilePath -> NExpr -> m (NValue m)
nixEvalExpr mpath = nixEval mpath id Eval.eval

-- so we don't need all that just yet, since I don't know what MonadNix is for, and Has for...

architectEval :: (Functor f) => Transform f (m a) -> Alg f (m a) -> Fix f -> a

-- so this seems like the signature of catamorphism
-- Functor f => (f a -> a) -> Fix f -> a
-- not sure..
type Transform f a = (Fix f -> a) -> Fix f -> a

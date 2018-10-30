{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

import Data.Bifunctor
import Data.Either
import Data.Void
import Text.Show.Functions

infixl 6 :+:
type a :+: b = Either a b

swap :: a :+: b -> b :+: a
swap = either Right Left

type i :->: o = Resumption i o
newtype Resumption i o = Resumption (i -> (o, Resumption i o)) deriving (Show)

-- sequential composition
infixl 6 <->
(<->) :: Resumption a b -> Resumption b c -> Resumption a c
(<->) (Resumption f) (Resumption g) = Resumption $ \a ->
  let (b, f') = f a
      (c, g') = g b
  in  (c, f' <-> g')

-- parallel composition
infixl 7 <|>
(<|>) :: (Resumption a b) -> (Resumption c d) -> Resumption (a :+: c) (b :+: d)
(<|>) (Resumption f) (Resumption g) = Resumption $ \case
  Left  a -> let (b, f') = f a in (Left b, f' <|> Resumption g)
  Right c -> let (d, g') = g c in (Right d, (Resumption f) <|> g')

-- apply a resumption
(<$$>) :: Resumption i r -> i -> (r, Resumption i r)
(<$$>) (Resumption f) i = f i

idResumption :: Resumption a a
idResumption = Resumption $ \a -> (a, idResumption)

-- the Resumption category is a monoidal category
-- :+: is the Bifunctor
-- Void is the identity object

-- "lambda" natural transformation in a monoidal category
leftUnitor :: Resumption (Void :+: a) a
leftUnitor = Resumption $ \case
  Left v -> (absurd v)
  Right a -> (a, leftUnitor)

leftUnitor' :: Resumption a (Void :+: a)
leftUnitor' = Resumption $ \a -> (Right a, leftUnitor')

-- "rho" natural transformation in a monoidal category
rightUnitor :: Resumption (a :+: Void) a
rightUnitor = Resumption $ \case
  Left a -> (a, rightUnitor)
  Right v -> absurd v

rightUnitor' :: Resumption a (a :+: Void)
rightUnitor' = Resumption $ \a -> (Left a, rightUnitor')

-- "alpha" natural transformation in a monoidal category
associator :: Resumption ((a :+: b) :+: c) (a :+: (b :+: c))
associator = Resumption $ \case
  Left  (Left  a) -> (Left a, associator)
  Left  (Right b) -> (Right (Left b), associator)
  Right c         -> (Right (Right c), associator)

associator' :: Resumption (a :+: (b :+: c)) ((a :+: b) :+: c)
associator' = Resumption $ \case
  Left a -> (Left (Left a), associator')

-- the Resumption category is a symmetric monoidal category

symmetric :: Resumption (a :+: b) (b :+: a)
symmetric = Resumption $ \e -> (swap e, symmetric)

-- the Resumption category is traced monoidal category

-- recursion
trace :: Resumption (i :+: r) (o :+: r) -> Resumption i o
trace f = Resumption $ \i -> loop f (Left i)
  where
    loop (Resumption f) i = case f i of
      (Left  o, f') -> (o, trace f')
      (Right r, f') -> loop f' (Right r)

-- laws on traced monoidal categories
-- these are not fully implemented because i'm lazy
-- yanking law: trace symmetric == id
-- naturality in X
-- naturality in Y
-- dinaturality in U
-- vanishing I
-- superposing



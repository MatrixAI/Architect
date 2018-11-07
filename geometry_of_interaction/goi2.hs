{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Bifunctor
import           Data.Either
import           Data.Void
import           Text.Show.Functions

infixl 6 :+:
type a :+: b = Either a b

swap :: a :+: b -> b :+: a
swap = either Right Left

newtype Resumption i o = Resumption (i -> (o, Resumption i o)) deriving (Show)
infixl 5 :->:
type i :->: o = Resumption i o

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
applyRes :: Resumption i r -> i -> (r, Resumption i r)
applyRes (Resumption f) i = f i

idResumption :: Resumption a a
idResumption = Resumption $ \a -> (a, idResumption)

-- the Resumption category is a monoidal category
-- :+: is the Bifunctor
-- Void is the identity object

-- "lambda" natural transformation in a monoidal category
leftUnitor :: Resumption (Void :+: a) a
leftUnitor = Resumption $ \case
  Left  v -> (absurd v)
  Right a -> (a, leftUnitor)

leftUnitor' :: Resumption a (Void :+: a)
leftUnitor' = Resumption $ \a -> (Right a, leftUnitor')

-- "rho" natural transformation in a monoidal category
rightUnitor :: Resumption (a :+: Void) a
rightUnitor = Resumption $ \case
  Left  a -> (a, rightUnitor)
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

-- this is also a morphism in C
-- the mapping between 1 interface to another
-- represented as an Resumption encoding
-- and is also a morphism
-- Interaction a a' b b' is a morphism (a, a') -> (b b') that is in G(C)
-- but that is also a Resumption morphism in C itself
newtype Interaction a a' b b' = Interaction (Resumption (a :+: b') (a' :+: b))

applyInt
  :: Interaction a a' b b' -> a :+: b' -> (a' :+: b, Interaction a a' b b')
applyInt (Interaction f) i = let (o, f') = applyRes f i in (o, Interaction f')

-- a proper interaction is more like
-- Interaction a b b a
-- what process left sends needs to be what process right receives
-- but this interaction is not done


idInteraction :: Interaction a a' a a'
idInteraction = Interaction symmetric

-- sequential interaction composition
{-
          +----+     +----+
    +     |    |     |    |     +
    | A+  | B- |     |    | B+  | C+
    |     |    |     |    |     |
  +-v-----v-+  |     |  +-v-----v-+
  |         |  ++   ++  |         |
  |    f    |   +-^-+   |    g    |
  |         |  ++   ++  |         |
  +-+-----+-+  |     |  +-+-----+-+
    |     |    |     |    |     |
    | A-  | B+ |     |    | B-  | C+
    v     |    |     |    |     v
          +----+     +----+

f and g are interactions
here we sequentially compose 2 interactions
this means we actually 3 "processes"
where the middle process interacts with 2 other processes
this can represent A <-> B <-> C
where B becomes a hidden process
and the entire interaction becomes just between A and C
this entire interaction becomes self contained
we only need to care about A's input and output
and C's input and output
the assoc1 and assoc2 is what allows
-}
(<=>) :: Interaction a a' b b' -> Interaction b b' c c' -> Interaction a a' c c'
(<=>) (Interaction f) (Interaction g) =
  Interaction $ trace $ assoc1 <-> (f <|> g) <-> assoc2
 where
  assoc1 :: Resumption ((a :+: c') :+: (b' :+: b)) ((a :+: b') :+: (b :+: c'))
  assoc1 = Resumption $ \case
    Left  (Left  a ) -> (Left (Left a), assoc1)
    Left  (Right c') -> (Right (Right c'), assoc1)
    Right (Left  b') -> (Left (Right b'), assoc1)
    Right (Right b ) -> (Right (Left b), assoc1)
  assoc2 :: Resumption ((a' :+: b) :+: (b' :+: c)) ((a' :+: c) :+: (b' :+: b))
  assoc2 = Resumption $ \case
    Left  (Left  a') -> (Left (Left a'), assoc2)
    Left  (Right b ) -> (Right (Right b), assoc2)
    Right (Left  b') -> (Right (Left b'), assoc2)
    Right (Right c ) -> (Left (Right c), assoc2)

-- parallel interaction composition
(<||>)
  :: Interaction a a' b b'
  -> Interaction c c' d d'
  -> Interaction (a :+: c) (a' :+: c') (b :+: d) (b' :+: d')
(<||>) (Interaction f) (Interaction g) =
  Interaction $ (assoc1 <-> (f <|> g) <-> assoc2)
 where
  assoc1 :: Resumption ((a :+: c) :+: (b' :+: d')) ((a :+: b') :+: (c :+: d'))
  assoc1 = Resumption $ \case
    Left  (Left  a ) -> (Left (Left a), assoc1)
    Left  (Right c ) -> (Right (Left c), assoc1)
    Right (Left  b') -> (Left (Right b'), assoc1)
    Right (Right d') -> (Right (Right d'), assoc1)
  assoc2 :: Resumption ((a' :+: b) :+: (c' :+: d)) ((a' :+: c') :+: (b :+: d))
  assoc2 = Resumption $ \case
    Left  (Left  a') -> (Left (Left a'), assoc2)
    Left  (Right b ) -> (Right (Left b), assoc2)
    Right (Left  c') -> (Left (Right c'), assoc2)
    Right (Right d ) -> (Right (Right d), assoc2)

dualRes :: Resumption (a :+: b) (c :+: d) -> Resumption (b :+: a) (d :+: c)
dualRes f =
  Resumption $ \i -> let (o, f') = applyRes f (swap i) in (swap o, dualRes f')

dualInt :: Interaction a a' b b' -> Interaction b' b a' a
dualInt (Interaction f) = Interaction $ dualRes f

curryRes
  :: Resumption ((a :+: b) :+: c) ((d :+: e) :+: f)
  -> Resumption (a :+: (b :+: c)) (d :+: (e :+: f))
curryRes f = Resumption $ \i -> bimap swapO curryRes (applyRes f $ swapI i)
 where
  swapI = either (Left . Left) (either (Left . Right) Right)
  swapO = either (either Left (Right . Left)) (Right . Right)

curryInt
  :: Interaction (a :+: b) (c :+: d) e f -> Interaction a c (d :+: e) (b :+: f)
curryInt (Interaction f) = Interaction $ curryRes f

uncurryRes
  :: Resumption (a :+: (b :+: c)) (d :+: (e :+: f))
  -> Resumption ((a :+: b) :+: c) ((d :+: e) :+: f)
uncurryRes f = Resumption $ \i -> bimap swapO uncurryRes (applyRes f $ swapI i)
 where
  swapI = either (either Left (Right . Left)) (Right . Right)
  swapO = either (Left . Left) (either (Left . Right) Right)

uncurryInt
  :: Interaction a b (c :+: d) (e :+: f) -> Interaction (a :+: e) (b :+: c) d f
uncurryInt (Interaction f) = Interaction $ uncurryRes f

-- to do this, you need dependent types
-- interfaces are purely type level constructs here
-- newtype Interface a b = Interface (a, b)
-- interact :: Interface a a' -> Interface b b' -> Interaction a a' b b'

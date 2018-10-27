{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Bifunctor
import           Data.Either
import           Data.Void

-- $ => (.)
-- sum => Either
-- case => either
-- sum => bimap
-- abort => absurd (I think this is right)
-- type zero = Void of zero => newtype Void = Void Void

-- it appears to be a recursive type (an infinite newtype results in a newtype that can never contain itself)
-- it's just not possible to construct... oh that's interesting
-- wait haskell already has this with "EmptyDataDeclarations"
-- but I guess this means you don't need the extension
-- so it appears that we can use a Void type to
-- cancel out possible paths and ensure that no code can be written
-- that produces such a thing
-- it's a weird roundabout but it allows us to say that certain things are not possible when the program is fully compiled!

type a :+ b = Either a b

swap :: Either a b -> Either b a
swap = either Right Left


-- now we have resumption
-- R = I -> (O * R)
-- this is written with Ocaml modules
-- we need to write this using some sort of structure
-- type ('i, 'o) r = R of ('i -> 'o * ('i, 'o) r)
-- ocaml types are undercased and constructors are uppercased
-- types appear before the final constructor
-- almost in the appearance of what it needs or gives
-- you give a i then o, then you get a r... sort of thing
-- not sure what the * means here
-- i think it means tuple

-- type ('i, 'o) r = R of ('i -> 'o * ('i, 'o) r)
newtype R i o = R (i -> (o, R i o))

type i :-> o = R i o

-- data R i o = R {
--   resume :: i -> (o, R i o)
-- }


-- ok we have a Resume type
-- it's pretty straightforward to port, cool

-- (* val id : ('a, 'a) r *)
-- let rec id = R(fun x -> (x, id))
idR :: R o o
idR = R (\x -> (x, idR))


-- identity resumption
-- that's the idea
-- even allocate is that...
-- that's why the state is also recursive
-- but state action is s -> (a, s)
-- whereas this is R i o = (i -> (o, R i o))
-- it's slightly different....

-- in the coroutine work, the idR is generalised to the type level functor
-- that's called the `s` suspension functor
-- it's also further generalised to support a runtime monad
-- and then a result type

-- whereas the above is just a function that returns some result, and itself
-- a possibly different function but with the same input and output types

-- composing 2 resumptions
-- oh lol...

(=>>) :: R a b -> R b c -> R a c
(=>>) (R f) (R g) = R $ \x ->
  let (y, f') = f x
      (z, g') = g y
  in  (z, f' =>> g')

-- with composition we have R the category of resumptions
-- this category is also monoidally closed with the tensor product A*C
-- as the sum type A + C
-- what does this mean?
-- a closed category simply means that it is possible to map the homset between a pair of objects to a single object in that category
-- the associative binary operator must return the same type
-- so what is the tensor product of the resumption category?
-- we only hava a composition operator
-- is it talking about Either?
-- why would that be still in the same category?

-- think of A and C as message types
-- then how to react to A and C message simulataneously?
-- so 2 resumptions are 2 different message types?
-- so we have f : A -> B and g: C -> D
-- then we can combine them by taking the A + C message
-- and dispatching to f or g
-- ooh.. so it's saying Either (R a b) (R c d)
-- is something as well?

-- here's our product operation

-- this doesn't make as much sense as before
-- because this is saying I can take A or C
-- and return B or D
-- but really what happens is that if I take A, then I return B
-- and if I take C then I return D
-- so by specialising AorC to A you already determine what the output type is
-- so it's Either (R a b) (R c d)
-- but that would not be a monoidally closed category
-- still I find this weird

-- (* val ( ** ) : ('a, 'b) r -> ('c, 'd) r -> (('a, 'c) sum, ('b, 'd) sum) r *)
(<=>) :: R a b -> R c d -> R (Either a c) (Either b d)
(<=>) (R f) (R g) = R $ \case
  Left  x -> let (y, f') = f x in (Left y, f' <=> R g)
  Right x -> let (y, g') = g x in (Right y, R f <=> g')


-- now we could also implement feedback
-- in the concept of resumptions
-- does that mean once I get the result
-- I just discard the rest of the function
-- this is not the function category
-- this is the "resumption" category
-- so actually the output is weird, cause it ALWAYS have to be another function
-- right now there's no terminal construction of the resumption
-- as in normally we would have a "Done" constructor to resemble termination

-- ok so monoidal product is <=>
-- composition is =>>
-- is now a monoidal closed category
-- is it symmetric?
-- no cause R a b <=> R c d is not the same as R c d <=> R a b?
-- the first gives back R (Either a c) (Either b d)
-- the second gives back R (Either c a) (Either b d)

-- the symmetric thing requires to be commutative
-- the order must not matter
-- but IT can work
-- since you can do a swap operation on the Either type
-- so it is equal up to isomorphism
-- so then it is symmetric
-- ooo lol
-- naturally isomorphic

-- this is now definitely a symmetric monoidal category
-- but is it a closed category? that depends...
-- morphisms between resumptions must be captured as a resumption itself
-- sort of... but we are adding the trace operator now
-- many closed monoidal categories are symmetric, however this need not be the case

-- homomorphic set between A*X and B*X gets turned homomorphic set between A and B in the same category
-- wiat A*X is the Either type!?
-- but... I thought A and X is the resumptions themselves, the category of resumptions contains resumptions as objects right?
-- that's like saying R itself is the category and the objects are the fundamental types
-- if that's the case then R constructor constructs a morphism
-- Tr : C(A*X, B*X) -> C(A, B)



-- the category is then just R, and the objects are the Hask types, the morphisms is constructed from R and the product operator is Either which is commutative up to isomorphism using the swap operator, the product operator is a functor... and its a symmetric functor due to natural isomorphism of swap, which applies onto the Functor
-- so then =>> and <=> operator on the category itself...
-- they are not what we are thinking?
-- or are we saying one is a monoidal closed category, and the other is a symmetric monoidal category!?

-- so R is in a monoidal closed category (not necessarily symmetric using =>> and <=>)
-- while the individual Hask types are a symmetric monoidal category?
-- so if the symmetric monoidal category gets equipped with trace operator, then we get GoI?


-- no I reread so it the article is explicitly saying that in this category, the objects are Hask types, the morphisms are R resumptions, the monoidal tensor product is Either



trace :: R (Either i r) (Either o r) -> R i o
trace f = R $ \i -> loop f (Left i)
 where
  loop (R f) v = case f v of
    (Left  o, f') -> (o, trace f')
    (Right r, f') -> loop f' (Right r)

-- so we get i to o by repeatedly sending the r back in until we get a o
-- there is definitely some relationship of this to fix
-- and it's almost making it really explicit
-- factorial' :: (Int -> Int) -> Int -> Int
-- fix :: (a -> a) -> a

-- Int, R -> Int, R


-- R (Either i r)    (Either o r) -> R i o
--   (a            -> a)          -> a
--   ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)


-- it's like as if R (Either i r) (Either o r)
-- is like a different encoding, a somewhat more EXPLICIT encoding of recursive functions
-- the point being is that the rec function itself is able to refer back to itself

-- ok... we have trace
-- we need the various associativity and commutativity properties
-- so these are just standard names for the natural transformations associated with a tensor product in cat theory

-- so this is just a resumption that keeps swapping between a or b on each resumption
sym :: R (Either a b) (Either b a)
sym = R $ \i -> (swap i, sym)

-- here is an resumption that takes either a or void, so it has to take a, and returns that a as output
rho :: R (Either a Void) a
rho = R $ \case
  Left  i -> (i, rho)
  Right v -> absurd v

rho' :: R a (Either a Void)
rho' = R $ \i -> (Left i, rho')

lambda :: R (Either Void a) a
lambda = R $ \case
  Left  v -> absurd v
  Right i -> (i, lambda)

lambda' :: R a (Either Void a)
lambda' = R $ \i -> (Right i, lambda')

-- wait if R isn't the objects in the category
-- instead R resumptions are the morphisms

-- the lambda is a natural isomorphism in the monoidal category
-- same with rho
-- these are respectivel ycalled the left and right unitor

-- lambda - left unitor
-- rho  - right unitor
-- the I identity object has a left and right idnetity
-- lambda A: I * A === A
-- rho A: A * I === A

-- wait the lambda and lambda' here express natural isomoprhisms
-- that is (Either Void a) => a and a => (Either Void a)
-- Here the tensor product is the Either bifunctor
-- the identity is Void

-- oh... identity object type for the either bifunctor is the Void type
-- they are natural isomorphisms because they map between functor to functor
-- either is the bifunctor, the monoidal laws are expressed as natural transformations

-- associator natural transformation takes A,B,C and gives back (A*B)*C === A*(B*C)
-- so you express this by doing
-- associator and associator'
-- so that's sym here (no sym is not, it's the commutative law)
-- sym has resumption of Either a b to Either b a
-- And R being a resumption gives us

-- the alpha natural transformation!
-- THIS IS the associative law
-- (* val alpha : ((('a, 'b) sum, 'c) sum, ('a, ('b, 'c) sum) sum) r *)

alpha :: ((a :+ b) :+ c) :-> (a :+ (b :+ c))
alpha = R $ \case
  Left  (Left  a) -> (Left a, alpha)
  Left  (Right b) -> (Right (Left b), alpha)
  Right c         -> (Right (Right c), alpha)

alpha' :: (a :+ (b :+ c)) :-> ((a :+ b) :+ c)
alpha' = R $ \case
  Left  a         -> (Left (Left a), alpha')
  Right (Left  b) -> (Left (Right b), alpha')
  Right (Right c) -> (Right c, alpha')

-- why is it a natural transformation
-- because the resumption morphism for alpha and alpha' is produced by mapping between 2 either functors (in fact it's Either of Either) functors

-- the G construction in terms of the resumption module we've just defined
-- the G is constructed from R
-- where R is the category above
-- the basic idea is here to talk about the bidirectional communication
-- so for objects in our new category G
-- there will be pairs of objects from our old one
-- in the new category of G
-- the objects will be pairs of the prior objects
-- but the previous objects are just types
-- are we saying we have 2 different types now?

-- objects of G will be pairs (A+, A-)
-- morphisms of the G is f: (A+, A-) -> (B+, B-)
-- so these are resumptions of A+ :+ B- :-> A- :+ B+
-- didn't we already have this?

-- wait... (A+, A-) is now A: :+ B-
-- huh?

-- A+ is type of A message sent out
-- A- is the type of A message received

-- A+ :+ B-
-- is therefore, the type of A message sent out AND the type of B message received

-- are we combining type of A messages sent out with the type B message being received?

-- we are transforming A messages out to B message sout
-- we need to accept B messages in order to transform them into A messagse in
-- whaat?

-- the objects of this category represent interfaces
-- morphisms is how you can map 1 interface to another
-- why is the resumption different?

-- it appears that G module "opens" the Resumption module
-- and so we bring all identifiers into scope

-- this is the morphism
-- the minimal definition of of a category is one which has id
-- the id is the identity morphism and and . is the composition operator
-- so yea... a category is completely defined by the morphism
-- so we only need to write the morphism, and then we got it

-- type ('i,'o) r = R of ('i -> 'o * ('i,'o) r)

newtype G a b' a' b = G (R (Either a b') (Either a' b))

-- so now G is the morphism!
-- and it's something that takes 4 types

-- and then the Identity
-- identity morphism?
gid :: G b a' a' b
gid = G sym
-- this isn't the identity object
-- this is the identity morphism!
-- oh so identity morphisms are polymorphic!

-- sym and is some swapping morphism?

-- wait so A+ * B- -> A- * B+ IS the morphism (A+, A-) -> (B+, B-)
-- why is this way?
-- I don't get it...
-- maybe it's like if I am sending out A, I'm also receiving A
-- and if I am receiving B, then I am sending out B
-- but why is this flipped?

-- I'm not sure why the decided to implement the morphisms of (A+, A-) -> (B+, B-)
-- as resumptions of
-- (A+ + B-) -> (A- + B+)
-- I just don't understand why it is flipped right now

-- ok to implement composition
-- we are doing f . g
-- where f :: (A+, A-) -> (B+, B-)
-- g :: (B+, B-) -> (C+, C-)
-- but remember we are doing it as resumptions

-- here it is inresumptions
-- f :: (A+ + B-) -> (A- + B+)
-- g :: (B+ + C-) -> (B-, C+)

-- we feed f's B+ to g's B+
-- and g's B- to f's B-
-- so we use the trace operator
-- it's like the `B` here is the recursion?

{--
 let rec assoc :
((
  ('aplus,'cminus) sum,
  ('bminus,'bplus) sum
) sum,
(
  ('aplus, 'bminus) sum,
  ('bplus, 'cminus) sum
) sum)
r

--}


-- weirdly the c' goes to the far right here!!
--              +-------+
--              |       |
assoc :: ((a :+ c') :+ (b' :+ b)) :-> ((a :+ b') :+ (b :+ c'))
assoc = R $ \case
  Left  (Left  a ) -> (Left (Left a), assoc)
  Left  (Right c') -> (Right (Right c'), assoc)
  Right (Left  b') -> (Left (Right b'), assoc)
  Right (Right b ) -> (Right (Left b), assoc)

--       +----------+
--       |          |
-- (A' + B) + (B' + C) -> (A' + C) + (B' + B)
assoc2 :: ((a' :+ b) :+ (b' :+ c)) :-> ((a' :+ c) :+ (b' :+ b))
assoc2 = R $ \case
  Left  (Left  a') -> (Left (Left a'), assoc2)
  Left  (Right b ) -> (Right (Right b), assoc2)
  Right (Left  b') -> (Right (Left b'), assoc2)
  Right (Right c ) -> (Left (Right c), assoc2)



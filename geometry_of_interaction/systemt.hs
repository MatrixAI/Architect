-- newtype Hom a b = Hom (a -> b)

type Hom a b = a -> b

idT :: Hom a a
idT = id

-- compose :: Hom a b -> Hom b c -> Hom a c

-- unit :: Hom a ()

-- pair :: Hom a b -> Hom a c -> Hom a (b, c)

-- fst :: Hom (a, b) a

-- snd :: Hom 

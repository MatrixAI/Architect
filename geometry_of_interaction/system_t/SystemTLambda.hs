

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

-- we are just going to use a normal error
-- here the type echecking does something interesting
-- here our type checker checks the combination of both the term and type
-- note that `return unit`
-- this means the unit in the example is a AST macro system using `<:expr<Goedel.unit >>`
-- but we are just going to use the `unit` directly!
-- in that case it's a Hom a ()
-- and that's a function type!
-- how weird!
-- especially it should be a expr!
-- let unit = <:expr< Goedel.unit >>
-- yep it doesn't really make sense
-- since the actua lthing is always an Expr
-- so I think we are always returning some sort of Expr


check :: TTerm -> TType -> Context TTerm
check tterm ttype = case (tterm ttype) of
  (Lam x e, Arrow ttype1 ttype2) -> undefined
  (Lam _ _, ttype) -> error $ "Unexpected function type " ++ showTType ttype
  (Unit, One) -> return unit

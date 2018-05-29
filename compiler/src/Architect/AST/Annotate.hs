{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TemplateHaskell    #-}

module Architect.AST.Annotate where

-- this allows us to annotate the AST
-- which we are able to do because of using the recursion scheme setup
-- so it's really a sort of an extension of the AST itself

-- import           Control.DeepSeq      (NFData)
import qualified Architect.AST        as AST
import           Data.Fix             (Fix (..))
import           Data.Function        (on)
import           Data.Functor.Compose (Compose (..))
import           Data.Semigroup       (Semigroup, (<>))
import           GHC.Generics         (Generic, Generic1)
import qualified Text.Megaparsec.Pos  as MPP
import qualified Text.Show.Deriving   as SD

-- | Generic annotation functor
-- ann is the annotation and the a is the annotated
-- Note that @Annotate ann@ is equivalent to a functor
data AnnotateF a b = AnnotateF
    { annotation :: a
    , annotated  :: b
    } deriving (
      Show,
      Read,
      Eq,
      Ord,
      Functor,
      Foldable,
      Traversable,
      Generic,
      Generic1
    )

$(SD.deriveShow1 ''AnnotateF)
$(SD.deriveShow2 ''AnnotateF)

-- | Generic functor composition for AnnotateF
-- We use recursion schemes here again, where the result @b@
-- Will be the thing being annotated
type ComposeAnnotateF ann f = Compose (AnnotateF ann) f

-- | Utility function that is useful with ComposeAnnotateF
-- Basically it will be used to convert annotations that contain
-- ComposeAnnotateF, and just wrap it with Fix and Compose
-- It's just boilerplate
fixCompose
  :: AnnotateF ann (f (Fix (ComposeAnnotateF ann f)))
  -> Fix (ComposeAnnotateF ann f)
fixCompose = Fix . Compose

-- SPECIFIC ANNOTATIONS for AST:

-- | Source location annotation
data SourceLoc = SourceLoc
  { begin :: MPP.SourcePos
  , end   :: MPP.SourcePos
  } deriving (Show, Eq, Ord, Generic)

instance Semigroup SourceLoc where
  s1 <> s2 = SourceLoc ((min `on` begin) s1 s2)
                       ((max `on` end) s1 s2)

-- | AST functor annotated with SourceLoc
-- Then fixed version using explicit recursion scheme
type ASTLocF = ComposeAnnotateF SourceLoc AST.ASTF
type ASTLoc = Fix ASTLocF

-- pattern AnnE :: forall ann (g :: * -> *).
--   ann -> g (Fix (Compose (Ann ann) g)) -> Fix (Compose (Ann ann) g)
-- pattern AnnE ann a = Fix (Compose (Ann ann a))

-- this joins 2 Fix Compose
-- it makes use fo AnnE as a bidirectional pattern
-- what's the usage of AnnE

-- this takes 2 ASTLoc
-- and comebines them up
-- into 1 ASTLoc, that represents the a binary application operator
-- this is a strange function to put into the annotate
-- it's only ever used by the parser
-- I think this function should be in the parser


-- these are basically high level combinators working on annotated AST trees
-- that is, they allow one to "build up" the annotated AST tree
-- from lower level trees
-- but the way you build them up depends on what the root operator is
-- and "how" you deal with their annotations, which in this case is location annotations
-- they are an algebra operating over annotated AST
-- so they are not part of the parser
-- but remember that this is an extension of the AST

-- here is the apply combinator
-- taking 2 subtrees, and generating an "apply AST"

-- these are all application operators
-- or combinators that join together ASTLoc with something
-- they build it up

apply :: ASTLoc -> ASTLoc -> ASTLoc
apply e1@(Fix (Compose (AnnotateF s1 _))) e2@(Fix (Compose (AnnotateF s2 _))) =
  Fix $ Compose $ AnnotateF (s1 <> s2) (AST.ASTBinary AST.OpApply e1 e2)

-- this gets used by the unary operators
-- so it's another AST combinator!
-- this is applying from a unary operator
unaryApply :: AnnotateF SourceLoc AST.UnaryOp -> ASTLoc -> ASTLoc
unaryApply (AnnotateF s1 u) e1@(Fix (Compose (AnnotateF s2 _))) =
  Fix $ Compose $ AnnotateF (s1 <> s2) (AST.ASTUnary u e1)


-- nUnary (AnnotateF s1 u) e1@(AnnotateF s2 _) = AnnE (s1 <> s2) (NUnary u e1)
-- nUnary _ _ = error "nUnary: unexpected"

-- this one is not very symmetrical
-- it doesn't directly take an ASTLoc
-- instead it ensures that you have some Annotated AST
-- note that it's really recursive definition
-- it's because this is with the annotateLocation function
-- it could be used using still ASTLoc
-- but you would need to convert it
-- note that the fact that Params contains ASTLoc is just because Params is explicitly recursive
-- so this is combinator as well, but now operating on a "leaf" part of the tree
-- well not exactly, it's a node/leaf within the tree
-- but... not the entire tree
-- it's one of the wrappers within the tree
-- the tree is very wrapped, we have compose functors, ASTF, Fix, annotations, annotation fix
-- there is a LOT of wrappers around this AST in order to give it higher order functionality
-- abstract :: AnnotateF SourceLoc (Params ASTLoc) -> ASTLoc -> ASTLoc

-- nApp :: NExprLoc -> NExprLoc -> NExprLoc
-- nApp e1@(AnnE s1 _) e2@(AnnE s2 _) = AnnE (s1 <> s2) (NBinary NApp e1 e2)
-- nApp _ _ = error "nApp: unexpected"

-- AnnE s1 _
-- AnnE ann a <=> Fix (Compose (Ann ann a))

-- Fix (Compose (AnnotationF s1 _))


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
import           Data.Functor.Compose (Compose (..))
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

-- | AST functor annotated with SourceLoc
-- Then fixed version using explicit recursion scheme
type ASTLocF = ComposeAnnotateF SourceLoc AST.ASTF
type ASTLoc = Fix ASTLocF

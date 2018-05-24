{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE KindSignatures     #-}

module Architect.Annotations where

import           Architect.Expr.Types
import           Control.DeepSeq      (NFData)
import           Data.Fix             (Fix (..))
import           Data.Functor.Compose (Compose (..))
import           GHC.Generics
import qualified Text.Megaparsec.Pos  as MPP

-- generic annotation data type
-- that basically 2 data types
-- the first is the annotation, the second is the annotated
data Annotate ann a = Annotate
    { annotation :: ann
    , annotated  :: a
    }
    deriving (
      Show,
      Read,
      Eq,
      Ord,
      Generic,
      Generic1,
      Functor,
      Foldable,
      Traversable,
      NFData)

-- this is a generic annotation fix
-- but we want to fix annotation to location
fixAnnotation :: Annotate ann (g (Fix (Compose (Annotate ann) g))) -> Fix (Compose (Annotate ann) g)
fixAnnotation = Fix . Compose

-- specific usage of annotations (annotating it with source positions)

data SrcSpan = SrcSpan
  { spanBegin :: MPP.SourcePos
  , spanEnd   :: MPP.SourcePos
  } deriving (Show, Eq, Ord, Generic, NFData)

-- composition of Annotating with SrcSpan with the AASTF as the other functor
-- because AASTF and Annotate SrcSpan are both functors
-- we compose the functor, and also state that the internal element is going to be fixed
-- to have explicit recursion
type AASTLocF = Compose (Annotate SrcSpan) AASTF

-- abstract syntax tree that is annotated with locations (or the span)
type AASTLoc = Fix AASTLocF


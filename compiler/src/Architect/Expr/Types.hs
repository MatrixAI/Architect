{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Architect.Expr.Types where

-- here we define types for Architect expressions
-- these are not architect types (which is its own thing)
-- but the haskell types for architect expressions
-- that is the AST

import           Control.DeepSeq
import           Data.Data
import           Data.Fix                   (Fix)
import           Data.Functor.Compose
import Data.Fix
import           Data.Hashable
import qualified Data.List.NonEmpty         as NE
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           GHC.Generics
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Error      as MPE
import qualified Text.Megaparsec.Pos        as MPP

data AASTF r = ASTLiteral ALit
             | ASTString (AStr r)
             | ASTName AName
             | ASTList [r]
             | ASTAttrSet [ABinding r]
             | ASTLet [ABinding r] r
             | ASTIf r r r

type AAST = Fix AASTF

data ALit = LitInt Integer
          | LitDouble Double
          deriving (Show, Eq, Ord, Generic)

data AStr r = StrQuoted [AText Text r]
            | StrIndented Int [AText Text r]
            deriving (Show, Eq, Ord, Generic)

data AName = NameAlpha Text
           | NameSymbol Text
           deriving (Show, Eq, Ord, Generic)

data ABinding r = Binding (AAttrPath r) r MPP.SourcePos
                deriving (Show, Eq, Ord, Generic, Generic1)

type AAttrPath r = NE.NonEmpty (AKey r)

data AKey r = KeyDynamic (AText (AStr r) r)
            | KeyStatic AName
            deriving (Show, Eq, Ord, Generic)


data AText (s :: *) (r :: *) = TextPlain s
                             | TextEscapedNewline
                             | TextAntiquoted r
                             deriving (Show, Eq, Ord, Generic, Generic1)

-- ok the above compiles
-- continue...

data SrcSpan = SrcSpan
  { spanBegin :: MPP.SourcePos
  , spanEnd   :: MPP.SourcePos
  } deriving (Show, Eq, Ord, Generic)

-- generic annotation data type
-- that basically 2 data types
-- the first is the annotation, the second is the annotated
data Annotate ann a = Annotate
    { annotation :: ann
    , annotated  :: a
    }
    deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable, Read)

-- composition of Annotating with SrcSpan with the AASTF as the other functor
-- because AASTF and Annotate SrcSpan are both functors
-- we compose the functor, and also state that the internal element is going to be fixed
-- to have explicit recursion
type AASTLocF = Compose (Annotate SrcSpan) AASTF

-- abstract syntax tree that is annotated with locations (or the span)
type AASTLoc = Fix AASTLocF

-- we don't have AnnF anymore
-- because we inlined it into AASTLocF
-- this one takes
-- what the hell is AnnE
-- it's a pattern
-- annToAnnF :: Annotate ann (f (Fix (AnnF ann f))) -> Fix (AnnF ann f)
-- annToAnnF (Annotate ann a) = AnnE ann a

-- AnnE ann a = Fix (Compose (Ann ann a))

annToAnnF :: Annotate ann (g (Fix (Compose (Annotate ann) g))) -> Fix (Compose (Annotate ann) g)
annToAnnF (Annotate ann a) = Fix (Compose (Annotate ann a))

-- without an AnnF type, our annotations here are just transformed into a fixed of a compose of the annotate


-- ok now we can have functions that now parse the AASTLoc

-- float :: Parser Double
-- float = lexeme L.float

-- nixFloat :: Parser NExprLoc
-- nixFloat = annotateLocation1 (try (mkFloatF . realToFrac <$> float) <?> "float")

-- let's see how we can do this here




-- the parser doesn't just parse NExpr
-- it parses NExprLoc
-- parseNixText :: Text -> Result NExpr
-- parseFromText :: Parser a -> Text -> Result a
-- what is Failure Doc for
-- it appears that Leijen is used for documentation

-- the Result type makes use of Leijen, we're ignoring that for now

-- type Parser = MP.Parsec Void Text

-- data Result a = Success a | Failure a deriving Show

-- parseFromText is using Text
-- and megaparsec parse
-- and wrapping it in Failure or Success
-- this is not working properly, some complex types are being used here
-- parseFromText :: Parser a -> Text -> Result a
-- parseFromText p txt =
--   either (Failure . MPE.parseErrorPretty' txt) Success $ MP.parse p "<string>" txt


-- what is this? parse?
-- MegaParsec parse
-- it needs to take a parser to run, the string of the source file
-- then s for the input of the source
-- the either then applies success or failure on it


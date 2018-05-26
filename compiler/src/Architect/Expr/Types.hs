{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}

-- we need TemplateHaskell and use the Text.Show.Deriving
-- then we need to use $(deriveShow1 ''AASTF)
-- what the hell!?
{-# LANGUAGE TemplateHaskell #-}

-- so if you use a type level fix
-- the TF can be deriving Show
-- but after you do type T = Fix TF
-- then when you try to show it, you actually need a show instance for it
-- apparently it's called Show1
-- Show1 f => Show (Fix f)
-- so... we need Show1 for the TF
-- to do this, we need to derive Show1!


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

-- this brings in classes like Show1
-- for lifting Prelude classes to unary type constructors
-- basically useful for type level Fix data structures
-- from the transformers package
import           Data.Functor.Classes

-- Show1 and Show2
-- not sure how that's supposed to work

-- these are from derive-compat for autoderiving stuff the transformers classes
import           Text.Read.Deriving
import           Text.Show.Deriving

data AASTF r = ASTLiteral ALit
             | ASTString (AStr r)
             | ASTName AName
             | ASTList [r]
             | ASTAttrSet [ABinding r]
             | ASTLet [ABinding r] r
             | ASTIf r r r
             deriving (Show, Eq, Ord, Generic, Generic1)


type AAST = Fix AASTF

data ALit = LitInt Integer
          | LitFloat Double
          deriving (Show, Eq, Ord, Generic)

data AStr r = StrQuoted [AText Text r]
            | StrIndented Int [AText Text r]
            deriving (Show, Eq, Ord, Generic)

data AName = NameAlpha Text
           | NameSymbol Text
           deriving (Show, Eq, Ord, Generic)

-- I'm thinking it doesn't make sense to bind to a AAttrPath
-- so I'm going to change that
-- AAttrPath is AKey
-- AKey is a name of a key...
-- so we are just going to change to just the AKey
data ABinding r = Binding (AKey r) r MPP.SourcePos
                deriving (Show, Eq, Ord, Generic, Generic1)

-- so a binding is just a key
-- data ABinding r = Binding (AAttrPath r) r MPP.SourcePos
--                 deriving (Show, Eq, Ord, Generic, Generic1)

type AAttrPath r = NE.NonEmpty (AKey r)

-- are we saying that keys can be dynamic or static
-- static names are just variable names
-- but otherwise they can be string names
-- I'm not sure what that's supposed to mean!
data AKey r = KeyDynamic (AText (AStr r) r)
            | KeyStatic AName
            deriving (Show, Eq, Ord, Generic)

-- I'm not sure, but this looks equivalent

-- data NKeyName r
--   = DynamicKey !(Antiquoted (NString r) r)
--   | StaticKey !VarName
--   deriving (Eq, Ord, Generic, Typeable, Data, Show, Read, NFData,
--             Hashable)


data AText (s :: *) (r :: *) = TextPlain s
                             | TextEscapedNewline
                             | TextAntiquoted r
                             deriving (Show, Eq, Ord, Generic, Generic1)

-- we need to derive Show1
-- so that Fix works
-- this is template haskell using the deriving-compat package
$(deriveShow1 ''AASTF)
$(deriveShow1 ''AStr)
$(deriveShow1 ''AText)
$(deriveShow2 ''AText)
$(deriveShow1 ''ABinding)

-- there's no documentation for Data.Functor.Classes for the latest transformers package
instance Show1 AKey where
  liftShowsPrec sp sl p = \case
    KeyStatic t -> showsUnaryWith showsPrec "KeyStatic" p t
    KeyDynamic a ->
      showsUnaryWith
      (liftShowsPrec2
       (liftShowsPrec sp sl)
       (liftShowList sp sl)
       sp
       sl)
      "KeyDynamic"
      p
      a

-- ok this is BASICAlly... a show for instance for the type level Fix of AASTF
-- where we can now "show" the AText

-- we need it for the annotations as well

-- Show1 docs says that it lifts Show class to unary type constructors
-- why does this matter?
-- because you don't do Show Type
-- but Show (Type a)
-- whats the significance!?

-- instance Show a => Show (Maybe a)
-- is already defined
-- instance Show1 Maybe where showsPrec1
-- wait... this mayes "Maybe" a Showable type
-- not Maybe a
-- why?

-- something to do with recursion schemes

-- recursion scehems are just as essential to idiomatic functional programming as `for` and `while` are to idiomatic imperative programming



-- we cannot auto derive Show1 for AKey
-- somehow because that `r` occurs twice
-- for KeyDynamic

-- lifShowsPrec
-- remember that showsPrec
-- is what you need to do for show
-- the Prec is for precedence


-- oh it's not automatic!

-- there's still an error about how AKey doesn't have a Show1 instance
-- but AKey is part of

-- -- ok the above compiles
-- -- continue...

-- data SrcSpan = SrcSpan
--   { spanBegin :: MPP.SourcePos
--   , spanEnd   :: MPP.SourcePos
--   } deriving (Show, Eq, Ord, Generic)

-- -- generic annotation data type
-- -- that basically 2 data types
-- -- the first is the annotation, the second is the annotated
-- data Annotate ann a = Annotate
--     { annotation :: ann
--     , annotated  :: a
--     }
--     deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable, Read)

-- -- composition of Annotating with SrcSpan with the AASTF as the other functor
-- -- because AASTF and Annotate SrcSpan are both functors
-- -- we compose the functor, and also state that the internal element is going to be fixed
-- -- to have explicit recursion
-- type AASTLocF = Compose (Annotate SrcSpan) AASTF

-- -- abstract syntax tree that is annotated with locations (or the span)
-- type AASTLoc = Fix AASTLocF

-- fixAnnotation :: Annotate ann (g (Fix (Compose (Annotate ann) g))) -> Fix (Compose (Annotate ann) g)
-- fixAnnotation = Fix . Compose

-- we don't have AnnF anymore
-- because we inlined it into AASTLocF
-- this one takes
-- what the hell is AnnE
-- it's a pattern
-- annToAnnF :: Annotate ann (f (Fix (AnnF ann f))) -> Fix (AnnF ann f)
-- annToAnnF (Annotate ann a) = AnnE ann a

-- AnnE ann a = Fix (Compose (Ann ann a))


-- we know that an annotation is a Annotate of a Fix of a Compose
-- it's complicated yes...


-- fixed annToAnnF

-- would you say this unwraps or wraps
-- it extracts out Annotate ann a
-- only to rewrap it into Annotate ann a
-- so it's a bit weird
-- it's really wrapping the Fix and Compose again!

-- without an AnnF type, our annotations here are just transformed into a fixed of a compose of the annotate

-- the annToAnnF takes a type of Annotate ann (g Fix(Compose (Annotate ann) g))


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

-- this is deriveShow1
-- this is deriving it

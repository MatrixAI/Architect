{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Architect.AST where

-- these imports I haven't figured out yet
-- import           Control.DeepSeq
-- import           Data.Data
import           Data.Fix                   (Fix)
import           Data.Functor.Classes       (Show1)
import qualified Data.Functor.Classes       as FC
import qualified Data.List.NonEmpty         as NE
import           Data.Text                  (Text)
import           Data.Traversable           (Traversable)
import qualified Data.Traversable           as T
import           Data.Void                  (Void)
import           GHC.Generics               (Generic, Generic1)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Error      as MPE
import qualified Text.Megaparsec.Pos        as MPP
import qualified Text.Read.Deriving         as RD
import qualified Text.Show.Deriving         as SD

-- | Abstract Syntax Tree Functor to achieve f-algebra style type-level fix
-- otherwise known as "recursion-scheme"
data ASTF r = ASTLiteral Lit
            | ASTString (Str r)
            | ASTName Name
            | ASTList [r]
            | ASTAttrSet [Binding r]
            | ASTLet [Binding r] r
            | ASTIf r r r
            | ASTSelect r (KeyPath r)
            | ASTUnary UnaryOp r
            | ASTBinary BinaryOp r r
            deriving (Show, Eq, Ord, Functor, Generic, Generic1)

type AST = Fix ASTF

data Binding r = Binding (Key r) r MPP.SourcePos
               deriving (Show, Eq, Ord, Functor, Generic, Generic1)

data Lit = LitInt Integer
         | LitFloat Double
         deriving (Show, Eq, Ord, Generic)

data Str r = StrQuoted [Quote Text r]
           | StrIndented Int [Quote Text r]
           deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- | Quotes are the actual text
-- It can be newlines
-- A plain text
-- or Antiquoted text
-- Note that Str encapsulates Quote, but specifies s to be a string
-- Whereas a dynamic key asks for a quote, but specifies s to be a Str
data Quote (s :: *) (r :: *) = QuoteEscapedNewline
                             | QuotePlain s
                             | QuoteAntiquoted r
                             deriving (
                               Show,
                               Eq,
                               Ord,
                               Functor,
                               Foldable,
                               Traversable,
                               Generic,
                               Generic1
                             )

-- | Names are just variable names.
-- Names are not the same as keys, because keys can be string keys
-- Or antiquoted strings
data Name = NameAlpha Text
          | NameSymbol Text
          deriving (Show, Eq, Ord, Generic)

-- | KeyPath is a non empty list of keys.
-- Something like keya.keyb.keyc
type KeyPath r = NE.NonEmpty (Key r)

-- | Key is a name that can appear on the left hande side of an equals sign.
-- It can be assignment, but also bindings into an attribute set.
-- Because this type uses the recursion type @r@ multiple times.
-- We have to perform manual derivations of many type classes.
-- The auto derivations fail.
--
-- So we can have static keynames and dynamic keynames.
-- Static names are just literal identifiers.
-- Dynamic names are where they are strings.
-- It can be literal strings or quoted strings.
-- An antiquote, is not a quote that is embeding something else.
-- But antiquotes, saying that what ever is in it must be substituted with a binding.
-- `Quote` can be antiquoted.
--
-- This was derived from Nix's keys. Which are used for let bindings and
-- attribute sets. Which allows the usage of string keys, literal names,
-- and antiquoted strings.
data Key r = KeyStatic Name
           | KeyDynamic (Quote (Str r) r)
           deriving (Show, Eq, Ord, Generic)

instance Functor Key where
  fmap = T.fmapDefault

instance Traversable Key where
  traverse f = \case
    KeyDynamic (QuotePlain str)    -> KeyDynamic . QuotePlain <$> T.traverse f str
    KeyDynamic QuoteEscapedNewline -> pure $ KeyDynamic QuoteEscapedNewline
    KeyDynamic (QuoteAntiquoted e) -> KeyDynamic . QuoteAntiquoted <$> f e
    KeyStatic key                  -> pure (KeyStatic key)

instance Foldable Key where
  foldMap = T.foldMapDefault

instance Show1 Key where
  liftShowsPrec sp sl p = \case
    KeyStatic t  -> FC.showsUnaryWith showsPrec "KeyStatic" p t
    KeyDynamic a -> FC.showsUnaryWith
      (FC.liftShowsPrec2 (FC.liftShowsPrec sp sl) (FC.liftShowList sp sl) sp sl)
      "KeyDynamic" p a

-- a unary operator can just be overloaded
-- but for now we will just allow it
data UnaryOp = OpNeg | OpNot
  deriving (Show, Read, Eq, Ord, Generic)

data BinaryOp = OpEq
              | OpNEq
              | OpPlus
              | OpMinus
              | OpMult
              | OpDiv
              | OpApply -- Function application
              deriving (Show, Read, Eq, Ord, Generic)

-- Template Haskell Derivations
-- using derive-compat must come at the end
-- the order matters here

$(SD.deriveShow1 ''ASTF)
$(SD.deriveShow1 ''Binding)
$(SD.deriveShow1 ''Str)
$(SD.deriveShow1 ''Quote)
$(SD.deriveShow2 ''Quote)

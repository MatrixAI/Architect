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

module Architect.Expr.Types where

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

data AASTF r = ASTLiteral ALit
             | ASTString (AStr r)
             | ASTName AName
             | ASTList [r]
             | ASTAttrSet [ABinding r]
             | ASTLet [ABinding r] r
             | ASTIf r r r
             deriving (Show, Eq, Ord, Functor, Generic, Generic1)


type AAST = Fix AASTF

data ALit = LitInt Integer
          | LitFloat Double
          deriving (Show, Eq, Ord, Generic)

data AStr r = StrQuoted [AText Text r]
            | StrIndented Int [AText Text r]
            deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Generic)

data AName = NameAlpha Text
           | NameSymbol Text
           deriving (Show, Eq, Ord, Generic)

data ABinding r = Binding (AKey r) r MPP.SourcePos
                deriving (Show, Eq, Ord, Functor, Generic, Generic1)

type AAttrPath r = NE.NonEmpty (AKey r)


-- AText is actually about antiquoted
-- I'm thinking of changing the name to antiquoted so we are sure that this is really it
data AText (s :: *) (r :: *) = TextPlain s
                             | TextEscapedNewline
                             | TextAntiquoted r
                             deriving (
                               Show,
                               Eq,
                               Ord,
                               Functor,
                               Traversable,
                               Foldable,
                               Generic,
                               Generic1
                             )

-- | AKey is a name that can appear on the left hande side of an equals sign.
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
-- `AText` can be antiquoted.
--
-- This was derived from Nix's keys. Which are used for let bindings and
-- attribute sets. Which allows the usage of string keys, literal names,
-- and antiquoted strings.
data AKey r = KeyStatic AName
            | KeyDynamic (AText (AStr r) r)
            deriving (Show, Eq, Ord, Generic)

instance Functor AKey where
  fmap = T.fmapDefault

instance Traversable AKey where
  traverse f = \case
    KeyDynamic (TextPlain str)    -> KeyDynamic . TextPlain <$> T.traverse f str
    KeyDynamic TextEscapedNewline -> pure $ KeyDynamic TextEscapedNewline
    KeyDynamic (TextAntiquoted e) -> KeyDynamic . TextAntiquoted <$> f e
    KeyStatic key                 -> pure (KeyStatic key)

instance Foldable AKey where
  foldMap = T.foldMapDefault

instance Show1 AKey where
  liftShowsPrec sp sl p = \case
    KeyStatic t  -> FC.showsUnaryWith showsPrec "KeyStatic" p t
    KeyDynamic a -> FC.showsUnaryWith
      (FC.liftShowsPrec2 (FC.liftShowsPrec sp sl) (FC.liftShowList sp sl) sp sl)
      "KeyDynamic" p a

-- Template Haskell Derivations
-- using derive-compat must come at the end
-- the order matters here

$(SD.deriveShow1 ''AASTF)
$(SD.deriveShow1 ''AStr)
$(SD.deriveShow1 ''AText)
$(SD.deriveShow2 ''AText)
$(SD.deriveShow1 ''ABinding)

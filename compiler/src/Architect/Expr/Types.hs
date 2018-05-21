{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
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
import           Data.Hashable
import qualified Data.List.NonEmpty         as NE
import           Data.Text                  (Text)
import           GHC.Generics

import           Data.Void                  (Void)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Pos        as MPP
import qualified Text.Megaparsec.Error as MPE


data AASTF r = ASTLiteral ALit
             | ASTString (AStr r)
             | ASTName AName
             | ASTList [r]
             | ASTAttrSet [ABinding r]
             | ASTLet [ABinding r] r
             | ASTIf r r r

type AAST = Fix AASTF

data ALit = LitInt Integer
          | LitFloat Float
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

-- the parser doesn't just parse NExpr
-- it parses NExprLoc
-- parseNixText :: Text -> Result NExpr
-- parseFromText :: Parser a -> Text -> Result a
-- what is Failure Doc for
-- it appears that Leijen is used for documentation

-- the Result type makes use of Leijen, we're ignoring that for now

type Parser = MP.Parsec Void Text

data Result a = Success a | Failure a deriving Show

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


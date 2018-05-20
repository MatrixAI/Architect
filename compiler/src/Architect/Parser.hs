{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Architect.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char                  (isAlpha, isDigit, isSpace)
import qualified Data.HashSet               as HS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Pos        as MPP

-- haskell now has NonEmpty lists
import qualified Data.List.NonEmpty as NE

import Data.Functor.Compose

import           Control.Monad              (void)
import GHC.Generics
import Data.Data
import Data.Hashable

-- DeepSeq is for NFData so we can fully evaluate data structures
import Control.DeepSeq

type Parser = MP.Parsec Void Text

data SrcSpan = SrcSpan
  { spanBegin :: MPP.SourcePos
  , spanEnd   :: MPP.SourcePos
  } deriving (Show, Ord, Eq, NFData, Generic, Typeable, Data)

data Ann ann a = Ann
  { annotation :: ann,
    annotated :: a
  } deriving (Show, Ord, Eq, Generic, Generic1)

-- so here we have the Compose functor
-- it itself is also a Functor
-- it is the composition of 2 functors
-- it is a new type
-- so Ann ann is a Functor
-- f is also a Functor
-- if f is meant to be the fixed version, does that mean AnnF is then Fixed to include itself?
type AnnF ann f = Compose (Ann ann) f

-- so here we go, we are saying that SrcSpan is our ann
-- so the annotations take SrcSpan
-- the NExprF is what exactly?
type NExprLocF = AnnF SrcSpan NExprF

type NExprLoc = Fix NExprLocF

-- here we have the recursive expression type
-- we are giving types to the things we are parsing

-- do these things represent what are the types of the things
-- cause we have constants, string
-- symbol, list, set
-- recset, literlapath

-- The Architect Abstract Syntax Tree
-- what is a constant? it's not one of those, it's a literal
-- let's call it a literal or primitive
-- a literal
-- but it needs to take another type since we have many literals
-- oh yea a constant is used sometimes

-- constant, literal, atom... all means the same thing here

-- bools should be defined by a constructor...
-- wait we also have null?
-- noooo!
-- no nulls allowed!
-- use Maybe types!

data ALiteral = LitInt Integer
              | LitFloat Float

data AString = StringQuoted
             | StringIndented

type AVariable = Text

-- this is a list
-- a list of [...]
-- AttrPath because it's possible to do let a.b = 3
-- instead of just let a = 3
-- that's pretty weird
-- if you do that, a automatically becomes a attribute set
-- a non empty list of Keyname along with r

type AAttrPath r = NE.NonEmpty (AKeyName r)

-- a keyname is then a valid binding name

data ABinding r = VariableNamed (AttrPath r) r SourcePos

data AASTF r = ASTConstant ALiteral
             | ASTString AString
             | ASTSymbol AVariable
             | ASTList [r]
             | ASTAttrSet [ABinding r]
             | ASTUnary AOpUnary r
             | ASTBinary AOpBinary r r
             | ASTLet [ABinding r] r
             | ASTIf r r r

-- so we are saying that Ann is a functor...
-- its recursion is limited by using AnnF
-- instead of Ann
-- it's a form of type level Fix
-- oh wait, it being F means it can now be fixed
-- it is just a recursive type!

-- so yea... smart constructors are required!
-- MPP.SourcePos "filepath" (MPP.mkPos 1) (MPP.mkPos 1)

symbol :: Text -> Parser Text
symbol = MPL.symbol spaces

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme spaces

lineComment :: Parser ()
lineComment = MPL.skipLineComment "//"

blockComment :: Parser ()
blockComment = MPL.skipBlockComment "/*" "*/"

spaces :: Parser ()
spaces = MPL.space MPC.space1 lineComment blockComment

spacesInline :: Parser ()
spacesInline = MPL.space (void $ MP.takeWhile1P Nothing pred) lineComment blockComment where
  pred c = c == ' ' || c == '\t'

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

curlyBrace :: Parser a -> Parser a
curlyBrace = MP.between (symbol "{") (symbol "}")

squareBrace :: Parser a -> Parser a
squareBrace = MP.between (symbol "[") (symbol "]")

angleBrace :: Parser a -> Parser a
angleBrace = MP.between (symbol "<") (symbol ">")

reservedIdentifiers :: HS.Set Text
reservedIdentifiers = HS.fromList
  ["let", "in", "if", "then", "else"]

identifier :: Parser Text
identifier = lexeme $ MP.try $ do
  ident <- T.cons
    <$> (MPC.lowerChar <|> MPC.char '_')
    <*> MP.takeWhileP Nothing identLetter
  if HS.member ident reservedIdentifiers
    then fail $ "Cannot use reserved identifier: " ++ show ident
    else return ident
  where
    identLetter :: Char -> Bool
    identLetter x = isAlpha x || isDigit x || x == '_'


integer :: Parser Integer
integer = lexeme MPL.decimal

float :: Parser Double
float = lexeme MPL.float

-- special parsers
equals :: Parser Text
equals = symbol "="

comma :: Parser Text
comma = symbol ","

-- so this is saying that there can be an end to the reserved thing
-- also this n is matching just a n and then saying not to have anything else afterwards
-- so it's reserved
reservedEnd :: Char -> Bool
reservedEnd x =
  isSpace x ||
  x == '{'  ||
  x == '('  ||
  x == '['  ||
  x == '}'  ||
  x == ')'  ||
  x == ']'  ||
  x == ';'  ||
  x == ':'  ||
  x == '.'  ||
  x == '"'  ||
  x == '\'' ||
  x == ','

reserved :: Text -> Parser ()
reserved n = lexeme $ MP.try $
  MPC.string n *> MP.lookAhead (void (MPC.satisfy reservedEnd) <|> MP.eof)

-- we have a thing called nixTerm which parses a NExprLoc
-- what is NExprLoc?
-- it's in Nix/Expr/Types/Annotated.hs
-- type NExprLoc = Fix NExprLocF
-- type NExprLocF = AnnF SrcSpan NExprF
-- so there's some sort of position tracking maintained here
-- type AnnF ann f = Compose (Ann ann) f

-- data Ann ann a = Ann {
--   annotation :: ann
--   annotated :: a
--                      }

-- it appears to contain an annotation of type ann
-- and the annotated thing itself which is a
-- so what is ann?
-- it's SrcSpan
-- what is a?
-- it is NExprF
-- so the src span is some sort of annotation for keeping track of the text location
-- this is important...


-- the SourcePos has sourceName, sourceLine, sourcColumn
-- ok so we keep track of the name as well
-- so that's the file name
-- you can do sourcePosPretty and take a SourcePos and print out the string
-- FilePath is by the System.IO

-- source pos is the megaparsec type that keeps track of the position
-- Text.Megaparsec.Pos

-- ok I see it... "./abc:1:1"

-- SourcePos represents source positions, it contains the name of the source file
-- a line number and a column number
-- source line and column positions change intensively during parsing, so we need to make them strict to avoid memory leaks

-- if parser text parses empty
-- isn't that an error?
-- what does that mean?

-- these are the primitives within the language
-- our language is based on Nix, but with signifcant whitespace I think
-- or something else...
-- we also have top level expressions
-- so the top level is always a module
-- so you don't always need to wrap it in {}
-- it's just for readability

-- we need let in
-- what about operator overloading
-- we want to bring that in as well
-- so...
-- also what is true or false
-- we would want to allow data constructors right?
-- or do we not need this, creating algebraic data structures


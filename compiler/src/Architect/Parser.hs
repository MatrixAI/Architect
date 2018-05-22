{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Fix

-- haskell now has NonEmpty lists
import qualified Data.List.NonEmpty as NE

import Data.Functor.Compose

import           Control.Monad              (void)
import GHC.Generics
import Data.Data
import Data.Hashable

-- DeepSeq is for NFData so we can fully evaluate data structures
import Control.DeepSeq

import Architect.Expr.Types

type Parser = MP.Parsec Void Text

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

mkDoubleF :: Double -> AASTF r
mkDoubleF = ASTLiteral . LitDouble

-- so this annotates a parser
-- ok...
-- hmm this annotated position might be useful later not just for debugging but for manipulation as well...
-- I wonder if that's what they were thinking
annotateLocation :: Parser a -> Parser (Annotate SrcSpan a)
annotateLocation p = do
  posBegin <- MP.getPosition
  result   <- p
  posEnd   <- MP.getPosition
  return $ Annotate (SrcSpan posBegin posEnd) result

-- this one takes a parser of some AASTF...
-- but makes it parse AASTLoc instead
-- annotateLocation1 :: Parser (AASTF AASTLoc) -> Parser AASTLoc
-- annotateLocation1 = fmap annToAnnF . annotateLocation

-- annToAnnF uses pattern synonyms

-- I don't know how patterns are used with functions
-- are they just replacements
-- also since AnnF doesn't exist
-- then why do we have this?

-- architectFloat :: Parser AASTLoc
-- architectFloat = MP.try (mkDoubleF <$> float)

-- mkFloatF . realToFrac <$>

-- mkFloatF :: Float -> NExprF a
-- mkFloatF = NConstant . NFloat
-- these are constructors!
-- mkFloatF = ASTLiteral . LitFloat
-- this takes a float to 

-- nix only has floats
-- but we can do doubles easily
-- right...?
-- so what we are doing here?

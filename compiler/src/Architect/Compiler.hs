{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Architect.Compiler where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Data.Scientific as Sci
import qualified Text.Megaparsec.Char.Lexer as L

type Program = [Equation]

data Equation = Equation String Expr
  deriving (Eq, Show)

data Expr = Value Double
          | Reference String
          | Negation Expr
          | Sum Expr Expr
          | Subtraction Expr Expr
          | Multiplication Expr Expr
          | Division Expr Expr
          deriving (Eq, Show)

-- no custom error messages
-- not sure how the custom error messages here get used though

type Parser = Parsec Void String

-- 2 whitespace consuming parsers
-- scn is consumes newlines and whitespace
-- we will use it for whitespace between equations
-- equations are newline delimited

-- sc does not consume newlines, and is used to define lexemes
-- things that automatically eat whitespace after them

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

name :: Parser String
name = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "name"

float :: Parser Double
float = lexeme (Sci.toRealFloat <$> L.scientific)

expr :: Parser Expr
expr = makeExprParser term table

term :: Parser Expr
term = parens expr <|>
       (Reference <$> name) <|>
       (Value <$> float)

table :: [[Operator Parser Expr]]
table =
  [
    [Prefix (Negation <$ symbol "-")],
    [
      InfixL $ Multiplication <$ symbol "*",
      InfixL $ Division <$ symbol "/"
    ],
    [
      InfixL $ Sum <$ symbol "+",
      InfixL $ Subtraction <$ symbol "-"
    ]
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Equation where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Data.Scientific as Sci
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text, unpack)
import NeatInterpolation (text)

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

equation :: Parser Equation
equation = Equation <$> (name <* symbol "=") <*> expr

prog :: Parser Program
prog = between scn eof (sepEndBy equation scn)

-- we need the leading white space in the prog explicitly
-- because there can be leading whitespaces
-- megaparsec is meant to work on Text just as well
-- we'll have to explore later

textProg = do
  let source = unpack $ [text|
    a = 1 + 2
    b = a * 3
    c = a + b
  |]
  parseTest prog source

-- so our parser has nice error messages
-- every expression is separated by others in a newline
-- this spearation makes it possible to analyse many expressions
-- independently
-- even if one is malformed we have no reason to stop and not check the others
-- ok so what we are saying is that we should check ahead for more errors, than just stop on the first one
-- especially if errors are independent of each other
-- this is just syntax errors

type RawData t e = [Either (ParseError t e) Equation]

-- a list of Eithers, where it can be a parse error or equation

-- a collection of euqations, but every one of them may be malformed

-- in this case we get an error message on left

-- so ParserError on Char?
-- and e is Void
-- what!?
rawData :: Parser (RawData Char Void)
rawData = between scn eof (sepEndBy e scn)
  where
    e = withRecovery recover (Right <$> equation)
    recover err = Left err <$ manyTill anyChar eol
    -- TypeFamilies is required here
    -- to allow equational constraint

testRawData = do
  let source = unpack $ [text|
    foo = (x $ y) * 5 + 7.2 * z
    bar = 15
  |]
  parseTest rawData source

-- withRecovery runs the parser p as usual, but if it fails it takes ParseError
-- and provides it as an argument to `r`
-- in `r` we start right where `p` failed
-- no backtracking happens, because it would make it harder to find position from where to start normal parsing again

-- we eat all input up to the next newline

-- when the recovering parser `r` fails as well
-- I can see that what hapen sis that when the recovering happens
-- we use Left on err and map into the rest of the things
-- <$ replaces the inside with the erro
-- it appears to then return a Left of Trivial Error
-- the TrivialError takes a SourcePos, Just and fromList
-- the SourcePos has some position information
-- the Just appears to be the thing that failed
-- fromList appears to be the rest of the stuff
-- not entirely sure what :| is for

-- if the recovery fails, then the parser fails as normal
-- a recovering parser cannot influence error messages

-- what to do with RawData
-- you can either take all error messages and print them one by one
-- or ignore errors altogether and only filter to valid equations to work with
-- I'm still not sure what this RawData is

-- we have a Parser instead of a sequence of Equations
-- we have a sequence of Eithers, they could be a ParseError or the Equation
-- what to do with the ParseError?
-- it is Char token, and custom error data e
-- so no custom error data
-- but waht about the t, why is that Char?

-- fromList is a set!
-- it produces a set!
-- it is a stack of source positions, the unexpected token, and the expected tokens
-- so what we ahve is $ which is unexpected
-- then we have expected being ), o or t (o for operator) or (t for the rest of expression)
-- where did this label bome from?

-- ErrorItem t
-- it can be a non empty stream of tokens or a label

-- the :| is a list constructor it is for a non-empty and non strict list type
-- ok... so it's an error message?
-- what's the Label for?

-- withRecovery is just used so to allow parts of text to fail
-- the recovery then must be able to skip to some termination delimiter
-- in a language like python, indentation levels tell apart high level definitions
-- in every case, you should use your judgement
-- i can imagine withRecovery would be used for syntax highlighting
-- or other things that should work regardless of a single error

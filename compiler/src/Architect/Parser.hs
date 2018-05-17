{-# LANGUAGE OverloadedStrings #-}
module Architect.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char                  (isAlpha, isDigit)
import qualified Data.HashSet               as HS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

import           Control.Monad              (void)

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


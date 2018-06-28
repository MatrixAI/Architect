{-# LANGUAGE OverloadedStrings  #-}

module Architect.Parser.Syntax where

import           Control.Monad              (void, msum)
import           Control.Applicative
import qualified Data.HashSet               as HS
import           Data.Void                  (Void)
import           Data.Text                  (Text)
import qualified Data.Char                  as C
import qualified Data.Text                  as T
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Pos        as MPP
import qualified Text.Megaparsec.Expr       as MPE
import Architect.Parser.Types (Parser)

-- | Symbol parses a lexical unit
-- A lexical unit discards subsequent whitespace
symbol :: Text -> Parser Text
symbol = MPL.symbol spaces

-- | Combinator that wraps a parser into a lexical unit
-- A lexical unit discards subsequent whitespace
lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme spaces

-- | Line comments
lineComment :: Parser ()
lineComment = MPL.skipLineComment "//"

-- | Block comments
blockComment :: Parser ()
blockComment = MPL.skipBlockComment "/*" "*/"

-- | This is our whitespace parser, line comments and block comments are whitespace
-- This also parses nothing.
-- Don't use this when you are literally trying to parse some spaces.
spaces :: Parser ()
spaces = MPL.space MPC.space1 lineComment blockComment

parenBrace :: Parser a -> Parser a
parenBrace = MP.between (symbol "(") (symbol ")")

curlyBrace :: Parser a -> Parser a
curlyBrace = MP.between (symbol "{") (symbol "}")

squareBrace :: Parser a -> Parser a
squareBrace = MP.between (symbol "[") (symbol "]")

angleBrace :: Parser a -> Parser a
angleBrace = MP.between (symbol "<") (symbol ">")

doubleQuote :: Parser a -> Parser a
doubleQuote = MP.between (symbol "\"") (symbol "\"")

singleQuote :: Parser a -> Parser a
singleQuote = MP.between (symbol "'") (symbol "'")

equals :: Parser Text
equals = symbol "="

comma :: Parser Text
comma = symbol ","

semicolon :: Parser Text
semicolon = symbol ";"

colon :: Parser Text
colon = symbol ":"

integer :: Parser Integer
integer = lexeme MPL.decimal

float :: Parser Double
float = lexeme MPL.float

reserved :: Text -> Parser ()
reserved i = (lexeme . MP.try) $
  MPC.string i *> MP.lookAhead reservedAfter
  where
    reservedAfter = MPC.space1 <|> lineComment <|> blockComment <|> MP.eof

reservedIdentifiers :: HS.Set Text
reservedIdentifiers = HS.fromList
  ["let", "in", "if", "then", "else", "rec"]


-- ok identifier is standard and is basically running some ident
-- but ident has rules on what is allowed
-- what should the rules be?

-- identifier :: Parser Text
-- identifier = lexeme $ MP.try $ do
--   ident <- T.cons
--     <$> (MPC.lowerChar <|> MPC.char '_')
--     <*> MP.takeWhileP Nothing identLetter
--   if HS.member ident reservedIdentifiers
--     then fail $ "Cannot use reserved identifier: " ++ show ident
--     else return ident
--   where
--     identLetter :: Char -> Bool
--     identLetter x = C.isAlpha x || C.isDigit x || x == '_'

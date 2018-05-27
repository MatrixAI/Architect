{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Architect.Parser where

import           Control.Applicative
import           Control.Monad
import qualified Data.Char as C
import qualified Data.Maybe as M
import           Data.Fix
import qualified Data.HashSet               as HS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Pos        as MPP

import           Text.Megaparsec            ((<?>))


-- haskell now has NonEmpty lists
import qualified Data.List.NonEmpty         as NE
import Data.List.NonEmpty (NonEmpty(..))

import           Data.Functor.Compose

import           Control.Monad              (void)
import           Data.Data
import           Data.Hashable
import           GHC.Generics

-- DeepSeq is for NFData so we can fully evaluate data structures
import           Control.DeepSeq

import           Architect.Expr.Types
import Architect.Annotations

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
    identLetter x = C.isAlpha x || C.isDigit x || x == '_'

operator :: Parser Text
operator = lexeme $ MP.try $ do
  parens $ MP.takeWhile1P Nothing allowedSymbols
  where
    allowedSymbols s = (M.isNothing $ T.find (== s) disallowedSymbols)
      && (C.isPunctuation s || C.isSymbol s)

disallowedSymbols = "(),;[]`{}_:\"'"

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
  C.isSpace x ||
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

annotateLocation :: Parser a -> Parser (Annotate SrcSpan a)
annotateLocation p = do
  posBegin <- MP.getPosition
  result   <- p
  posEnd   <- MP.getPosition
  return $ Annotate (SrcSpan posBegin posEnd) result

-- converts a parser of the AST into a parser of an annotated AST with locations
annotateLocationAST :: Parser (AASTF AASTLoc) -> Parser AASTLoc
annotateLocationAST = fmap fixAnnotation . annotateLocation

archFloat :: Parser AASTLoc
archFloat = annotateLocationAST (floatToAST <$> float <?> "float")
 where
   floatToAST = ASTLiteral . LitFloat

archInt :: Parser AASTLoc
archInt = annotateLocationAST (intToAST <$> integer <?> "integer")
 where
   intToAST = ASTLiteral . LitInt

archName :: Parser AASTLoc
archName = annotateLocationAST ((ASTName . NameAlpha) <$> identifier)

archOperator :: Parser AASTLoc
archOperator = annotateLocationAST ((ASTName . NameSymbol) <$> operator)


-- to test archToplevel
-- we need to have archLet
-- and archIf
-- and archLamda
-- these are all recursive
-- so that's why we sort of needed it to work on the outset!

-- we are going to remove the the selector
-- because we don't know
archTopLevel :: Parser AASTLoc
-- archTopLevel = keywords <+> archLambda
--   where
--     keywords = archLet <+> archIf

archTopLevel = archLet <|> archInt

-- the way it works to actually resolve anything
-- is nixExprLoc
-- but if we don't have a table like that
-- then we just need to resolve to some sort of operator
-- still not sure why <|> or `mplus`
-- so we need to resolve to an operator

-- for some reason parseTest doesn't work on these things

-- right now let in let in let in... etc
-- but we need to resolve to something!
-- but we are not saying what it can be..


{-
let BINDINGS in
-}
-- this needs archBinders and archToplevel
archLet :: Parser AASTLoc
archLet = annotateLocationAST (reserved "let" *> letBinders <?> "let block")
  where
    letBinders = ASTLet <$> archBinders <*> (reserved "in" *> archTopLevel)

-- it is possible to bind nothing!
-- then it's a noop!

-- our binding will need semiclon
{-

all are allowed?
so it is optional
but significant whitespace is sort of important requiring indentation
to add a new one...


let
  a = blah
  b = blah

let a = blah; b = blah; in ...
let a = blah; b = blah in ...
-}

semi      = symbol ";"

archBinders :: Parser [ABinding AASTLoc]
archBinders = archBinding `MP.endBy` semi

-- Binding (AKey r) r MPP.SourcePos
-- a binding takes the name, the value it is bound to
-- and then the position
-- so that's hwy we have *>, which means we have a equal sign
-- but then we have the top level again!
-- and then we apply to the position!
-- note that *> discards the left, while <*> keeps the left in some way..

archBinding :: Parser (ABinding AASTLoc)
archBinding = do
  -- a binding has a position
  p <- MP.getPosition
  Binding <$> (keyStatic) -- keyname
          <*> (equals *> archTopLevel)   -- body that we are bound to
          <*> pure p
          <?> "variable binding!"
  where
    keyStatic = KeyStatic <$> ((NameAlpha <$> identifier) <|> (NameSymbol <$> operator)) -- this is meant to work


evaluate :: AAST -> Integer
evaluate = cata $ \case
  ASTLiteral (LitInt n) -> n
  ASTLiteral (LitFloat n) -> round n
  _ -> 0

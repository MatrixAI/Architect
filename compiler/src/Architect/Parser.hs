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


-- the top level form appears to be used as the actual top level expression here
-- it then sasys that with keywords
-- i'm not sure about <+>
-- it's a mplus operator
-- monad plus operator...
-- think of it like a monadic <|>
-- I wonder why they didn't use <|>
-- mzero is a parser that fails without consuming input

-- anyway, we try keywords, then try lambdas, then try nixExprLoc
-- keywords are let, if assert and with
-- so here it is, the top level expression to be parsed!!!

-- closely related to ALtenrative
-- so its basically both monad and alternative

-- what's this all about?
nixToplevelForm :: Parser NExprLoc
nixToplevelForm = keywords <+> nixLambda <+> nixExprLoc
  where
    keywords = nixLet <+> nixIf <+> nixAssert <+> nixWith

nixParens :: Parser NExprLoc
nixParens = parens nixToplevelForm <?> "parens"

nixLet :: Parser NExprLoc
nixLet = annotateLocation1 (reserved "let"
    *> (letBody <+> letBinders)
    <?> "let block")
  where
    letBinders = NLet
        <$> nixBinders
        <*> (reserved "in" *> nixToplevelForm)
    -- Let expressions `let {..., body = ...}' are just desugared
    -- into `(rec {..., body = ...}).body'.
    letBody = (\x -> NSelect x (StaticKey "body" :| []) Nothing) <$> aset
    aset = annotateLocation1 $ NRecSet <$> braces nixBinders

nixIf :: Parser NExprLoc
nixIf = annotateLocation1 (NIf
     <$> (reserved "if" *> nixExprLoc)
     <*> (reserved "then" *> nixToplevelForm)
     <*> (reserved "else" *> nixToplevelForm)
     <?> "if")

nixExprLoc :: Parser NExprLoc
nixExprLoc = makeExprParser nixTerm $ map (map snd) (nixOperators nixSelector)

nixLambda :: Parser NExprLoc
nixLambda = nAbs <$> annotateLocation (try argExpr) <*> nixToplevelForm

-- we have some functions that can parse AASTLoc
-- but we get an error when trying to use parseTest
-- No instance for (Data.Functor.Classes.Show1 (Annotate SrcSpan))
-- I think it has something to do with using the Compose functor
-- doesn't matter
-- we need to see a composion of these things


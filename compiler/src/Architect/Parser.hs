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
import Data.List.NonEmpty ((:|))

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

-- the body of the entire file
archBody :: Parser AASTLoc
archBody = undefined

-- archBinders similar to nixBinders
-- needs to parse a number of bindings
-- endBy means one or more namedVar separated by semi
-- where semi is a semicolon
-- err... we don't do that
-- we have useful whitespace
-- significant whitespace
-- so how would we do this!?

{-

A = Automaton {


}

B = Automaton {

}

-}

archBinders :: Parser [ABinding AASTLoc]
archBinders = many binding
  where
    -- this needs to acquire 1 binding
    binding = do
      p <- MP.getPosition
      Binding <$> (annotated <$> nixSelector)
              <*> (equals *> nixToplevelForm)
              <*> pure p
              <?> "variable binding"

-- AAttrPath is using NonEmpty lists
-- we need to turn it into a non empty lists
-- to do that we need to use :| as the consing operator
nixSelector :: Parser (Annotate SrcSpan (AAttrPath AASTLoc))
nixSelector = annotateLocation $ do
    (x:xs) <- keyName `sepBy1` selDot
    return $ x :| xs


-- they are all related to each other
-- we have keyName producing either a ydnamic key or a static key
-- a static key is like 'a'
-- a dynanmic key is like b
-- b can resolve to something
-- while 'a' resolves to just itself right...?
-- wait "dynamic identifiers" refer to just a string
-- or ${a} (a quoted string!?)
-- so.. dynamic names are keys for the current dictionary or whatever
-- but I wonder why they are called dynamic
-- it should be the other way around
-- we can have "literal" names or keys
-- or we can have dynamic names or keys that are refering to something else

-- { "a": 3 }
-- { a: 3}
-- which in nix means the same thing
-- to actually interpolate you have to do:
-- { ${a}: 3 }
-- whaat
-- also in Haskell we cannot do this
-- it's also because in haskell you cannot just created nested dictionaries
-- well you can, but that's a specific data structure
-- you don't just get { a ... etc}
-- you do get tuples
-- and lists
-- maps are explicitly defined
-- our constructors in Automaton
-- have:
{-

A = Automaton {
  protocol = ProtocolSpec
}

-}

-- so we do have special keys
-- but they represent the same as record constructs

-- so that means we don't have "..."
-- but if we allow arbitrary string keys, then we are not creating a record
-- so the question is that do we want to allow arbitrary records?
-- or does everything have to be defined according to a type? A record type?

-- we want to do it record style then...
-- that's what I was going for
-- but then our names cannot be arbitrary strings!

keyName :: Parser (AKey AASTLoc)
keyName = dynamicKey <+> staticKey
  where
    dynamicKey = KeyDynamic <$> nixAntiquoted nixString
    staticKey = KeyStatic <$> archName


-- this parsese annotated attribute path

-- annotated is the record selector
-- it is mapped to nixSelector
-- which I guess should give us a name right!?



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


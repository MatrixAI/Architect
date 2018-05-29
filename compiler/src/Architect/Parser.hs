{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Architect.Parser where

-- import           Control.DeepSeq
import qualified Architect.AST              as AST
import qualified Architect.AST.Annotate     as ASTA
import           Control.Applicative
import           Control.Monad
import           Control.Monad              (void)
import qualified Data.Char                  as C
import           Data.Data
import           Data.Fix
import           Data.Functor.Compose
import           Data.Hashable
import qualified Data.HashSet               as HS
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Maybe                 as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Text.Megaparsec            ((<?>))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Pos        as MPP
import qualified Text.Megaparsec.Expr       as MPE

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

semi :: Parser Text
semi = symbol ";"

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

-- HERE is where the parsers for AST is
-- it builds on top of the lower level parsers
-- in a way this should be in a different file...
-- not really
-- it's a different section basically
-- we should have a section comment

-- here we build up our annotations!
-- but we don't just parse the AST
-- we parse ASTLoc
-- that's the weirdest part
-- we extend AST with Location, and parse to that instead!

annotateLocation :: Parser a -> Parser (ASTA.AnnotateF ASTA.SourceLoc a)
annotateLocation p = do
  posBegin <- MP.getPosition
  result   <- p
  posEnd   <- MP.getPosition
  return $ ASTA.AnnotateF (ASTA.SourceLoc posBegin posEnd) result

annotateLocationAST :: Parser (AST.ASTF ASTA.ASTLoc) -> Parser ASTA.ASTLoc
annotateLocationAST = fmap ASTA.fixCompose . annotateLocation

archFloat :: Parser ASTA.ASTLoc
archFloat = annotateLocationAST
  ((AST.ASTLiteral . AST.LitFloat) <$> float <?> "float")

archInt :: Parser ASTA.ASTLoc
archInt = annotateLocationAST
  ((AST.ASTLiteral . AST.LitInt) <$> integer <?> "integer")

archName :: Parser ASTA.ASTLoc
archName = annotateLocationAST
  ((AST.ASTName . AST.NameAlpha) <$> identifier <?> "namealpha")

archOperator :: Parser ASTA.ASTLoc
archOperator = annotateLocationAST
  ((AST.ASTName . AST.NameSymbol) <$> operator <?> "namesymbol")

archTopLevel :: Parser ASTA.ASTLoc
archTopLevel = archLet <|> archInt

archLet :: Parser ASTA.ASTLoc
archLet = annotateLocationAST (reserved "let" *> letBinders <?> "let block")
  where
    letBinders = AST.ASTLet <$> archBinders <*> (reserved "in" *> archTopLevel)

archBinders :: Parser [AST.Binding ASTA.ASTLoc]
archBinders = archBinding `MP.endBy` semi

archBinding :: Parser (AST.Binding ASTA.ASTLoc)
archBinding = do
  -- a binding has a position
  p <- MP.getPosition
  AST.Binding <$> (keyStatic) -- keyname
              <*> (equals *> archTopLevel)   -- body that we are bound to
              <*> pure p
              <?> "binding"
  where
    keyStatic = AST.KeyStatic <$>
      (
        (AST.NameAlpha <$> identifier) <|>
        (AST.NameSymbol <$> operator)
      )

-- we need the if expression as well

archIf :: Parser ASTA.ASTLoc
archIf = annotateLocationAST
  (
    AST.ASTIf <$> (reserved "if" *> archExpr)
              <*> (reserved "then" *> archTopLevel)
              <*> (reserved "else" *> archTopLevel)
              <?> "if"
  )

-- here we go...
-- the nix one uses makeExprParser and makes up nixTerm
-- but if we have no operator precedence...
-- wait, are we saying we have no operators?
-- well in the beginning I was thinking about operator overloading
-- now I'm thinking this might be too complicated
-- however how would we do it if we parameterised our parsers based on whats...
-- so.. the parser can rely on some sort of state
-- what would that be?
-- maybe we start with makeExprParser
-- and then try to continue (and see how we can abtract it out)
-- doesn't that mean as we continue parsing, it needs to change the fixity
-- I need to know how makeExprParser works though
archExpr :: Parser ASTA.ASTLoc
archExpr = undefined

-- figuring out the expresssion system:....

-- a term is anything that isn't an operator
-- but what about straightforward function application?
-- and terms that are just symbols?
-- you allowed those to be names
-- so % is a valid name as a key
-- in that case % couldn't be an operator, unless it was spaced out

-- nixExprLoc :: Parser NExprLoc
-- nixExprLoc = makeExprParser nixTerm $ map (map snd) (nixOperators nixSelector)

-- -- pathChar is something that is Char -> Bool
-- -- apparently anything that can be in a path!
-- -- or we have those variants of braces
-- -- depending on the brace, we expect different data structures
-- -- nixSelect for parantheses
-- -- what is nixSelect it's some sort of attribute path

-- -- this is a lookahead, which means it doesn't consume anything

-- nixTerm :: Parser NExprLoc
-- nixTerm = do
--     c <- try $ lookAhead $ satisfy $ \x ->
--         pathChar x ||
--         x == '(' ||
--         x == '{' ||
--         x == '[' ||
--         x == '<' ||
--         x == '/' ||
--         x == '"' ||
--         x == '\''
--     case c of
--         '('  -> nixSelect nixParens
--         '{'  -> nixSelect nixSet
--         '['  -> nixList
--         '<'  -> nixSPath
--         '/'  -> nixPath
--         '"'  -> nixStringExpr
--         '\'' -> nixStringExpr
--         _    -> msum $
--             [ nixSelect nixSet | c == 'r' ] ++
--             [ nixPath | pathChar c ] ++
--             if isDigit c
--             then [ nixFloat
--                  , nixInt ]
--             else [ nixUri | isAlpha c ] ++
--                  [ nixBool | c == 't' || c == 'f' ] ++
--                  [ nixNull | c == 'n' ] ++
--                  [ nixSelect nixSym ]


-- -- WHITESPACE is an operator as well!
-- -- that's what the NBinaryDef!
-- -- it is in this operator table


-- here we define the operators that are relevant
-- if we want to overloaded operators, we would need to abstract this into a state
-- that can be changed by the parser
-- note that will also change the AST
-- we would need a generic operator ast
-- that can be specialised to whatever it needs to be
-- specifically whether the operator is unary or binary or whatever
-- depends on the type of the operator
-- but since we are not building a general purpose language, we'll leave this to the future!
archOperators :: [[MPE.Operator Parser ASTA.ASTLoc]]
archOperators =
  [
    [MPE.InfixL $ ASTA.apply <$ symbol ""], -- function application (with spaces)
    [MPE.Prefix $ opWithLoc "-" AST.OpNeg ASTA.unaryApply] -- unary application
  ]

opWithLoc :: Text -> o -> (ASTA.AnnotateF ASTA.SourceLoc o -> a) -> Parser a
opWithLoc name op f = do
    ASTA.AnnotateF ann _ <- annotateLocation $ operator2 name
    return $ f (ASTA.AnnotateF ann op)

-- this conflicts with the rest of the parser with regards to generic operators
-- we still don't understand this well enough to work with arbitrary operators in the langauage
-- so we will fix the number of operators directly (note that this operator is generic, but fixes it to specific things)
operator2 :: Text -> Parser Text
operator2 "-" = lexeme . MP.try $ MPC.string "-" <* MP.notFollowedBy (MPC.char '>')
operator2 "/" = lexeme . MP.try $ MPC.string "/" <* MP.notFollowedBy (MPC.char '/')
operator2 "<" = lexeme . MP.try $ MPC.string "<" <* MP.notFollowedBy (MPC.char '=')
operator2 ">" = lexeme . MP.try $ MPC.string ">" <* MP.notFollowedBy (MPC.char '=')
operator2 n   = symbol n

-- still not sure what the point of the annotated asts is for
-- since we probably want to parse it into a tree that maintains some isomorphism with the original text

-- fixing the operators like this is problematic
-- but we have a fixed set of operators

-- the term is meant to be a parser for stuff
-- the table lists the operators that are being utilised!

-- evaluate :: AAST -> Integer
-- evaluate = cata $ \case
--   ASTLiteral (LitInt n) -> n
--   ASTLiteral (LitFloat n) -> round n
--   _ -> 0

-- -- evaluate it like
-- -- evaluate $ Fix $ ASTLiteral $ LitInt 1

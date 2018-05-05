module Architect.Compiler where

-- the void function discards the result of evaluation
-- such as the return value of an IO action
import Control.Monad (void)

-- nothing inhabits the Void type
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

import qualified Text.Megaparsec.Char.Lexer as L

{-

The language is:

WHILE language

a ::= x | n | - a | a opa a
b ::= true | false | not b | b opb b | a opr a
opa ::= + | - | * | /
opb ::= and | or
opr ::= > | <

S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S

-}

-- what's this BExpr
-- this the b
-- BoolConst True | BoolConst False
-- Not (BoolConst True) | Not (BoolConst False)
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)

-- boolean operators
data BBinOp = And
            | Or
            deriving (Show)

-- relational operators
data RBinOp = Greater
            | Less
            deriving (Show)

-- arithmetic expressions
-- you have strings!
-- and InstConstant!
-- should have use literal

-- empty <|> a == a
-- so that's what it means!

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

-- remember how you did your statements as right recursive
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          deriving (Show)

-- which module brougth in Parsec?
-- Void type?

-- Parsec a b
-- the a is the error component
-- the b is the type of the input
-- the final type is what you return (what you are creating)
-- unless all architect expressions are symlinks to some place in the content addressed store
type Parser = Parsec Void String

-- input stream will be in the form of String
-- in the future, we would stream in things
-- and we have to resolve the impedance mismatch between filesystem state
-- and whether we can track the state in multiple places in the filesystem
-- as we are able to make them live "files"
-- live files in the sense of file watching
-- that's one way to do it

-- lexer
-- whitespace ignoring is needed
-- if we are lexing them into an abstract structure
-- but if we are dealing with them as trees and modifiable expressions
-- we need to keep track of them as trees of concrete trees
-- in that case you would need to work out the order index tree structure in haskell as well
-- that may be quite difficult.. later on

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

-- if we didn't hav block comments we can pass Control.Applicative.empty
-- it's na applicative empty f a
-- so that would be an empty parser, a parser that consumes empty
-- an empty parser parses nothing and finishes immediately

-- the spaceConsumer consumes it and returns nothing

-- here the whitespace is consumed after every lxeme automatically
-- but not before it
-- the megaparsec function has a whitespace function
-- that you can take, and it is expected not to take anything
-- this is really high level, which is pretty cool!

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- a symbol parser takes a string as an argument
-- anr parses the string and whitespace after it
-- wait so we always pass the spaceConsumer as the first argument?

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- ok I think I understand
-- sol what we are saying is that lexeme will always take space consumer first
-- but actually it means we parse any lexme and then any spaces
-- so the next thing can always be that token, instead of more whitespace!

integer :: Parser Integer
integer = lexeme L.decimal

-- here we are reusing the lexeme parser

semicolon :: Parser String
semicolon = symbol ";"

-- various operators we can use the symbol

-- so symbol is a helper to parse symbols
-- symbols are verbatim strings
-- so these are just primitive literal text
-- symbols are just text
-- but they are also single characters right?
-- no i think they can take multiple things

-- identifiers... can be variable!

-- the "string" already has try

reservedWord :: String -> Parser ()
reservedWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- seems like a negative lookahead

reservedWords :: [String]
reservedWords =
  [
    "if",
    "then",
    "else",
    "while",
    "do",
    "skip",
    "true",
    "false",
    "not",
    "and",
    "or"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

-- we parse a sequence of characters
-- where the first character is a letter
-- the rest is several characters where every one of them is a letter or digit
-- we need to check if it is a reserved word
-- if so we need to fail with a message
-- the try is useful for backtracking
-- which means the fail is evaluated
-- otherwise many identifier would fail on such identifiers instead of just stopping

-- does that mean if try fails then, then it would try something else before returning just the error message?

-- this is our langauge! (The WHILE language)
whileParser :: Parser Stmt
whileParser = between spaceConsumer eof stmt

-- we only get rid of whitespace after tokens

-- this is actually a sequence of statements
-- but potentially only 1 statement as well
-- I'm guessing sepBy1 must be returning multiple
stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semicolon
  where
    f l = if length l == 1 then head l else Seq l

stmt' :: Parser Stmt
stmt' = ifStmt <|>
        whileStmt <|>
        skipStmt <|>
        assignStmt <|>
        parens stmt

-- this means we need to try the longer match before the shorter match if the reserved words have the same prefix

-- where did sepBy1 come from?

ifStmt :: Parser Stmt
ifStmt = do
  reservedWord "if"
  cond <- bExpr
  reservedWord "then"
  stmt1 <- stmt
  reservedWord "else"
  stmt2 <- stmt
  return (If cond stmt1 stmt2)

whileStmt :: Parser Stmt
whileStmt = do
  reservedWord "while"
  cond <- bExpr
  reservedWord "do"
  stmt1 <- stmt
  return (While cond stmt1)

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  void (symbol ":=")
  expr <- aExpr
  return (Assign var expr)

skipStmt :: Parser Stmt
skipStmt = Skip <$ reservedWord "skip"


-- so we don't need try
-- still don't exactly understand where try is meant to be

-- expressions

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [
    [Prefix (Neg <$ symbol "-")],
    [
      InfixL (ABinary Multiply <$ symbol "*"),
      InfixL (ABinary Divide <$ symbol "/")
    ],
    [
      InfixL (ABinary Add <$ symbol "+"),
      InfixL (ABinary Subtract <$ symbol "-")
    ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [
    [Prefix (Not <$ reservedWord "not")],
    [
      InfixL (BBinary And <$ reservedWord "and"),
      InfixL (BBinary Or <$ reservedWord "or")
    ]
  ]

-- precedence is bind tightly, bind looser

-- a term is not an expression?

-- what is aterm here?

aTerm :: Parser AExpr
aTerm = parens aExpr <|>
        Var <$> identifier <|>
        IntConst <$> integer

bTerm :: Parser BExpr
bTerm = parens bExpr <|>
        (BoolConst True <$ reservedWord "true") <|>
        (BoolConst False <$ reservedWord "false") <|>
        rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (symbol ">" *> pure Greater) <|>
           (symbol "<" *> pure Less)

-- try parseTest p input
-- or parseTest' p input
-- displays the offending line

-- megaparsec already does error reporting
-- pretty cool!

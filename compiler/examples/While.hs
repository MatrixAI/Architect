module While where

import           Data.Void                  (Void)
import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as Parsec
import qualified Text.Megaparsec.Char       as CharParser
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Expr       as ExprParser

{-
  WHILE Language Grammar
  a   ::= x | n | - a | a opa a
  b   ::= true | false | not b | b opb b | a opr a
  opa ::= + | - | * | /
  opb ::= and | or
  opr ::= > | <
  S   ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S
-}

-- in haskell we represent the abstract syntax tree
-- or BNF grammar as algebraic data types

data ArithExpr = Var String
               | IntLit Integer
               | Neg ArithExpr
               | ArithBinary ArithOp ArithExpr ArithExpr
               deriving (Show)

data BoolExpr = BoolLit Bool
              | Not BoolExpr
              | BoolBinary BoolOp BoolExpr BoolExpr
              | RelBinary RelOp ArithExpr ArithExpr
              deriving (Show)

data ArithOp = Add
             | Subtract
             | Multiply
             | Divide
             deriving (Show)

data BoolOp = And
            | Or
            deriving (Show)

data RelOp = Greater
            | Less
            deriving (Show)

data Stmt = Seq [Stmt]
          | Assign String ArithExpr
          | If BoolExpr Stmt
            Stmt
          | While BoolExpr Stmt
          | Skip
          deriving (Show)

-- there is little separation between lexer and parser here
-- the operations of lexer and parser is interleaved
-- a parsing function at the very end calls lexing functions
-- you can see it through the usage of the `lexeme` function
-- by the `reserved` and `identifier` parser

-- the parser can always do the job of the lexer
-- however usually, the 2 are separated
-- so that the lexer can reduce the amount of information noise
-- the parser has to deal with

-- using Haskell megaparsec means you get flexibility
-- and with functional abstraction, the parsers
-- are not dealing with the low level noise
-- the "lexer" are just lower level parsers

-- so this is parser abstraction!
-- reduces the amount of things you need to deal with

-- also this can do single pass parsing
-- in terms of building up a symbol table
-- this can also be done by layering the monad
-- instead of just maintaining the state of the input stream
-- we can also maintain the state of a symbol table

type Parser = Parsec.Parsec Void String

-- here we have the "low level" lexical parsers

space :: Parser ()
space = Lexer.space CharParser.space1 lineComment blockComment
  where
    lineComment = Lexer.skipLineComment "//"
    blockComment = Lexer.skipBlockComment "/*" "*/"

symbol :: String -> Parser String
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

parens :: Parser a -> Parser a
parens = Parsec.between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme Lexer.decimal

semicolon :: Parser String
semicolon = symbol ";"

-- here we have the "mid level" parsers
-- dealing with more "composite" syntax

reserved :: String -> Parser ()
reserved w =
  (lexeme . Parsec.try)
  (CharParser.string w *>
   Parsec.notFollowedBy CharParser.alphaNumChar)

identifier :: Parser String
identifier = (lexeme . Parsec.try) (p >>= check)
  where
    p = (:) <$> CharParser.letterChar <*> Parsec.many CharParser.alphaNumChar
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x
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

-- here we have the "high level" parsers
-- doing double duty in doing "code generation"
-- they are not just parsing strings/integers
-- they are not parsing __into__ the abstract syntax tree

stmt :: Parser Stmt
stmt = f <$> Parsec.sepBy1 stmt' semicolon
  where
    f l = if length l == 1 then head l else Seq l
    stmt' = ifStmt <|>
            whileStmt <|>
            skipStmt <|>
            assignStmt <|>
            parens stmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  condition <- boolExpr
  reserved "then"
  stmt1 <- stmt
  reserved "else"
  stmt2 <- stmt
  return (If condition stmt1 stmt2)

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- boolExpr
  reserved "do"
  stmt1 <- stmt
  return (While cond stmt1)

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  symbol ":="
  expr <- arithExpr
  return (Assign var expr)

skipStmt :: Parser Stmt
skipStmt = Skip <$ reserved "skip"

boolExpr :: Parser BoolExpr
boolExpr = ExprParser.makeExprParser boolTerm boolOperators

boolTerm :: Parser BoolExpr
boolTerm = parens boolExpr <|>
           (BoolLit True <$ reserved "true") <|>
           (BoolLit False <$ reserved "false") <|>
           relExpr

boolOperators :: [[ExprParser.Operator Parser BoolExpr]]
boolOperators =
  [
    [ExprParser.Prefix (Not <$ reserved "not")],
    [
      ExprParser.InfixL (BoolBinary And <$ reserved "and"),
      ExprParser.InfixL (BoolBinary Or <$ reserved "or")
    ]
  ]

arithExpr :: Parser ArithExpr
arithExpr = ExprParser.makeExprParser arithTerm arithOperators

arithTerm :: Parser ArithExpr
arithTerm = parens arithExpr <|>
            Var <$> identifier <|>
            IntLit <$> integer

arithOperators :: [[ExprParser.Operator Parser ArithExpr]]
arithOperators =
  [
    [ExprParser.Prefix (Neg <$ symbol "-")],
    [
      ExprParser.InfixL (ArithBinary Multiply <$ symbol "*"),
      ExprParser.InfixL (ArithBinary Divide   <$ symbol "/")
    ],
    [
      ExprParser.InfixL (ArithBinary Add      <$ symbol "+"),
      ExprParser.InfixL (ArithBinary Subtract <$ symbol "-")
    ]
  ]

relExpr :: Parser BoolExpr
relExpr = do
  a1 <- arithExpr
  op <- relOperators
  a2 <- arithExpr
  return (RelBinary op a1 a2)

relOperators :: Parser RelOp
relOperators = (symbol ">" *> pure Greater) <|>
               (symbol "<" *> pure Less)

-- here we have the parser entry point
-- this will parse into our WHILE language AST

whileParser :: Parser Stmt
whileParser = Parsec.between space Parsec.eof stmt

-- some tests of our parser

main :: IO ()
main = do
  ast1 <- Parsec.parseTest whileParser
    "a := 3"
  ast2 <- Parsec.parseTest whileParser
    "\
    \a:= 1;\
    \while a < 4 do\
    \  a := 5;\
    \  if 4 < 3 then\
    \    skip\
    \  else\
    \    (b := 4)\
    \"
  print ast1
  print ast2

{-
Assign "a" (IntLit 3)
Seq [Assign "a" (IntLit 1),While (RelBinary Less (Var "a") (IntLit 4)) (Seq [Assign "a" (IntLit 5),If (RelBinary Less (IntLit 4) (IntLit 3)) Skip (Assign "b" (IntLit 4))])]
-}

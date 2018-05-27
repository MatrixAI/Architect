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

nixExprLoc :: Parser NExprLoc
nixExprLoc = makeExprParser nixTerm $ map (map snd) (nixOperators nixSelector)

-- pathChar is something that is Char -> Bool
-- apparently anything that can be in a path!
-- or we have those variants of braces
-- depending on the brace, we expect different data structures
-- nixSelect for parantheses
-- what is nixSelect it's some sort of attribute path

-- this is a lookahead, which means it doesn't consume anything

nixTerm :: Parser NExprLoc
nixTerm = do
    c <- try $ lookAhead $ satisfy $ \x ->
        pathChar x ||
        x == '(' ||
        x == '{' ||
        x == '[' ||
        x == '<' ||
        x == '/' ||
        x == '"' ||
        x == '\''
    case c of
        '('  -> nixSelect nixParens
        '{'  -> nixSelect nixSet
        '['  -> nixList
        '<'  -> nixSPath
        '/'  -> nixPath
        '"'  -> nixStringExpr
        '\'' -> nixStringExpr
        _    -> msum $
            [ nixSelect nixSet | c == 'r' ] ++
            [ nixPath | pathChar c ] ++
            if isDigit c
            then [ nixFloat
                 , nixInt ]
            else [ nixUri | isAlpha c ] ++
                 [ nixBool | c == 't' || c == 'f' ] ++
                 [ nixNull | c == 'n' ] ++
                 [ nixSelect nixSym ]


-- WHITESPACE is an operator as well!
-- that's what the NBinaryDef!
-- it is in this operator table

data Associativity = AssocNone | AssocLeft | AssocRight
  deriving (Show, Eq, Ord, Generic, NFData)

-- this was in Annotated.hs
-- what is this supposed to mean?
-- AnnE is a bidirectional pattern
-- <>
-- this does some sort of joining...
-- why?
-- <> is Semigroup joining operator
-- we are joining the source positions some how
-- well... start becomes the biggest start, and end is the biggest end
-- and so we then have... NBinary NApp e1 e2
-- what is the oint of this!?
-- it is just to join 2 Annotated AST's locations into 1 annotated AST!?
-- InfixL and InfixR comes from Megaparsec!
nApp :: NExprLoc -> NExprLoc -> NExprLoc
nApp e1@(AnnE s1 _) e2@(AnnE s2 _) = AnnE (s1 <> s2) (NBinary NApp e1 e2)
nApp _ _ = error "nApp: unexpected"

archOperators = [
  [
    (
      NBinaryDef " " OpApply AssocLeft,
      InfixL $ nApp <$ symbol ""
    )
  ]
                ]


nixOperators
    :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
    -> [[(NOperatorDef, Operator Parser NExprLoc)]]
nixOperators selector =
  [ -- This is not parsed here, even though technically it's part of the
    -- expression table. The problem is that in some cases, such as list
    -- membership, it's also a term. And since terms are effectively the
    -- highest precedence entities parsed by the expression parser, it ends up
    -- working out that we parse them as a kind of "meta-term".

    -- {-  1 -} [ (NSpecialDef "." NSelectOp NAssocLeft,
    --             Postfix $ do
    --                    sel <- seldot *> selector
    --                    mor <- optional (reserved "or" *> term)
    --                    return $ \x -> nSelectLoc x sel mor) ]

    {-  2 -} [ (NBinaryDef " " NApp NAssocLeft,
                -- Thanks to Brent Yorgey for showing me this trick!
                InfixL $ nApp <$ symbol "") ]
  , {-  3 -} [ prefix  "-"  NNeg ]
  , {-  4 -} [ (NSpecialDef "?" NHasAttrOp NAssocLeft,
                Postfix $ symbol "?" *> (flip nHasAttr <$> selector)) ]
  , {-  5 -} [ binaryR "++" NConcat ]
  , {-  6 -} [ binaryL "*"  NMult
             , binaryL "/"  NDiv ]
  , {-  7 -} [ binaryL "+"  NPlus
             , binaryL "-"  NMinus ]
  , {-  8 -} [ prefix  "!"  NNot ]
  , {-  9 -} [ binaryR "//" NUpdate ]
  , {- 10 -} [ binaryL "<"  NLt
             , binaryL ">"  NGt
             , binaryL "<=" NLte
             , binaryL ">=" NGte ]
  , {- 11 -} [ binaryN "==" NEq
             , binaryN "!=" NNEq ]
  , {- 12 -} [ binaryL "&&" NAnd ]
  , {- 13 -} [ binaryL "||" NOr ]
  , {- 14 -} [ binaryN "->" NImpl ]
  ]

-- this is passed into nixOperators
-- what does this mean?
nixSelector :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
nixSelector = annotateLocation $ do
    (x:xs) <- keyName `sepBy1` selDot
    return $ x :| xs

-- we have NOperatorDef
-- here being Unary, Binary and SpecialDef
-- ok so NBinaryDef is used with " " to mean application
-- then there's NApp which is what?

data NOperatorDef
  = NUnaryDef Text NUnaryOp
  | NBinaryDef Text NBinaryOp NAssoc
  | NSpecialDef Text NSpecialOp NAssoc
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)

-- this seems to be wrapping NOperatorDef
-- what is NAssoc about?
binaryN name op = (NBinaryDef name op NAssocNone,
                   InfixN  (opWithLoc name op nBinary))
binaryL name op = (NBinaryDef name op NAssocLeft,
                   InfixL  (opWithLoc name op nBinary))
binaryR name op = (NBinaryDef name op NAssocRight,
                   InfixR  (opWithLoc name op nBinary))
prefix  name op = (NUnaryDef name op,
                   Prefix  (manyUnaryOp (opWithLoc name op nUnary)))

data NSpecialOp = NHasAttrOp | NSelectOp
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData)



-- it appears that NAssoc tags an operator with 3 types of associativity

-- it is also using the AST Unary and Binary Op
-- that means our AST actually has specified operators representing specific constructs
-- should Automatons also be like that? yea... they would be privileged constructs, first class concepts as well!


-- the term is meant to be a parser for stuff
-- the table lists the operators that are being utilised!

-- evaluate :: AAST -> Integer
-- evaluate = cata $ \case
--   ASTLiteral (LitInt n) -> n
--   ASTLiteral (LitFloat n) -> round n
--   _ -> 0

-- -- evaluate it like
-- -- evaluate $ Fix $ ASTLiteral $ LitInt 1

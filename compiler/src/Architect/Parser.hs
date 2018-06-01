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
import           Control.Monad              (void, msum)
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
  $ MP.try ((AST.ASTLiteral . AST.LitFloat) <$> float <?> "float")

archInt :: Parser ASTA.ASTLoc
archInt = annotateLocationAST
  ((AST.ASTLiteral . AST.LitInt) <$> integer <?> "integer")



archName :: Parser ASTA.ASTLoc
archName = annotateLocationAST
  ((AST.ASTName . AST.NameAlpha) <$> identifier <?> "namealpha")

archOperator :: Parser ASTA.ASTLoc
archOperator = annotateLocationAST
  ((AST.ASTName . AST.NameSymbol) <$> operator <?> "namesymbol")

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

-- nixSelect nixSym -- THIS MUST BE THE NAME
-- although strings are pretty complex



-- NSym

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
archExpr = MPE.makeExprParser archTerm archOperators


-- the only operator at the moment is whitespace
-- which means function application!
archOperators :: [[MPE.Operator Parser ASTA.ASTLoc]]
archOperators = [[MPE.InfixL $ ASTA.apply <$ symbol ""]]

archTopLevel :: Parser ASTA.ASTLoc
archTopLevel = archLet <|> archInt

-- when using msum order matters
-- especially with things that need to be tried
archTerm :: Parser ASTA.ASTLoc
archTerm = do
  c <- MP.try
    $ MP.lookAhead
    $ MPC.satisfy
    $ \c -> C.isAlpha c || C.isDigit c || c == '['
  case c of
    '[' -> archList
    _   -> msum
      $ if C.isDigit c then [archFloat, archInt] else [archName]

archOperator = annotateLocationAST
  ((AST.ASTName . AST.NameSymbol) <$> operator <?> "namesymbol")

-- wait this is meant to be nixStringExpr
archStringExpr :: Parser ASTA.ASTLoc
archStringExpr = annotateLocationAST archString

-- the reason this is outside, is that it gets used by something else
-- that doesn't want the ASTLoc although I'd argue that's pointless
-- continue with strings...
archString :: Parser (AST.Str ASTA.ASTLoc)
archString = lexeme (doubleQuoted <|> indented <?> "string")
  where
    doubleQuoted = StrQuoted . removePlainEmpty . mergePlain
                   <$> (doubleQ *> many (stringChar doubleQ (void $ char '\\') doubleEscape) <* doubleQ)
    indented = undefined

archList :: Parser ASTA.ASTLoc
archList = annotateLocationAST
  (squareBrace (AST.ASTList <$> many archTerm) <?> "list")

-- so here we are parsing strings!


-- the reason it's so weird, is cause we are using annotateLocation
-- note annotateLocationAST
-- which should be producing an ast
-- annotateLocation just produces the annotate functor
-- whereas annotateLocationAST produces the ASTLoc (which is a composed functor of ASTF and AnnotateF with Fix applied to it)
-- if we are usign annotateLocation
-- it is because we are annotating a parser
-- in this  case Parser (Text)
-- or whatever
-- actually nixString is :: Parser (NString NExprLoc)
-- so I'm wondering why not just use annotateLocationAST
-- annotateLocationAST :: Parser (AST.ASTF ASTA.ASTLoc) -> Parser ASTA.ASTLoc
-- as you can see, as long as something is parsing the AST, we can just use annotateLocationAST
-- instead it wants to
-- nixStringExpr :: Parser NExprLoc
-- nixStringExpr = nStr <$> annotateLocation nixString
--use the nStr function
-- nStr :: Ann SrcSpan (NString NExprLoc) -> NExprLoc
-- nStr  (Ann s1 s) = AnnE s1 (NStr s)
-- it's just Fix Compose of Ann ann a
-- lol... this is basically just annotateLocationAST




-- nixTerm :: Parser NExprLoc
-- nixTerm = do
--     -- if the char is neither of these things
--     -- then what happens?
--     -- usually the parser fails... so are we saying all nixTerms must be
--     -- either pathChar, ( or { or ... etc
--     -- all those have special things
--     -- but pathChar is saying it can be alpha... etc
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
--         '('  -> nixSelect (parens nixToplevelForm) -- expanded it since it wasn't important
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
--             then [ nixFloat , nixInt ]
--             else [ nixUri | isAlpha c ] ++
--                  [ nixBool | c == 't' || c == 'f' ] ++
--                  [ nixNull | c == 'n' ] ++
--                  [ nixSelect nixSym ]

-- {-

-- nixToplevelForm :: Parser NExprLoc
-- nixToplevelForm = keywords <+> nixLambda <+> nixExprLoc
--   where
--     keywords = nixLet <+> nixIf <+> nixAssert <+> nixWith
-- -}

-- -- evaluate :: AAST -> Integer
-- -- evaluate = cata $ \case
-- --   ASTLiteral (LitInt n) -> n
-- --   ASTLiteral (LitFloat n) -> round n
-- --   _ -> 0

-- -- -- evaluate it like
-- -- -- evaluate $ Fix $ ASTLiteral $ LitInt 1

-- -- satisfy is :: (Token s -> Bool) -> m (Token s)
-- -- lookahead :: m a -> m a
-- -- so we have a predicate matching if the x is a pathCar, or `(` or `{`...
-- -- so we have a try...

-- -- there's a `pathChar` asking if the token is a path character?

-- -- why are all these available characters?





-- -- there are available characters
-- -- it just means if we have () they get nixSelect nixParens


-- nixSelector :: Parser (Ann SrcSpan (NAttrPath NExprLoc))
-- nixSelector = annotateLocation $ do
--     (x:xs) <- keyName `sepBy1` selDot
--     return $ x :| xs

-- nixSelect :: Parser NExprLoc -> Parser NExprLoc
-- nixSelect term = do
--     res <- build <$> term <*> optional ((,) <$> (selDot *> nixSelector)
--                           <*> optional (reserved "or" *> nixTerm))
--     continues <- optional $ lookAhead selDot
--     case continues of
--         Nothing -> pure res
--         Just _  -> nixSelect (pure res)
--  where
--   build :: NExprLoc
--         -> Maybe (Ann SrcSpan (NAttrPath NExprLoc), Maybe NExprLoc)
--         -> NExprLoc
--   build t Nothing = t
--   build t (Just (s,o)) = nSelectLoc t s o

-- optional :: f a -> f (Maybe a)

-- why is continues?
-- it is optional applied to a lookAhead of selDot

-- so it tries to see if we havea selDot and it's NOT followed bya nixPath
-- like whaaat?
-- if we continue... then we return a pure res which is whatever that was
-- if there is SOMETHING, then we run nixSelect on pure res again
-- so what is this saying

-- ({a = 3;}).a or 3 .a
-- is it something like this?
-- but wouldn't that get captured by the nixSelector?
-- how is that possible!?
-- cause the term (term nixToplevelForm) which should already get the ()
-- how can there be more dots!?

-- nix allows `({ a = { a = 3; }; }).a     .a`
-- the whitespace doesn't matter, and its not a syntax error
-- that's what's happening there
-- that's why there's an extra continues
-- and allows a recursive application
-- cause I think that the nixSelector

-- so I get what nixSelect does
-- and that's because the point of nix expressions is that the whole thing is meant to resolve to anything
-- i still think this sort of abstraction is strange
-- its wrapping general terms (which is a recursive application of nixToplevelForm)
-- with an optional wrapping of selection on it


-- the term is nixParens
-- nixSelect is very abstract
-- the term which is nixParens
-- is just parens nixToplevelForm
-- ooo.. so it's actually recursive
-- nixSelect nixParens
-- is actually
-- nixSelect (parens nixToplevelform)
-- the parens takes in something
-- but why pass it to nixSelect?

-- build takes in ::
-- ASTLoc ->
-- Maybe (AnnotateF SourceLoc (AttrPath ASTLoc), Maybe ASTLoc) ->
-- ASTLoc
-- so a Maybe of a tuple
-- where that tuple has a left of an Annotated structure containing an attribute path
-- and Maybe an ASTloc
-- then if Nothing, we just return the original ASTLoc
-- if Just (s, o)... we use `nSelectLoc t s o`
-- it's another annotated ast combinator taking astloc, annotated attribute path and a maybe astaloc
-- what the hell is this!?
-- what is NSelect?

-- NSelect is for dot reference into an attribute set...
-- it has r (NAttrPath r) (Maybe r)
-- why?
-- apparently

-- so I'm getting it, the first thing is the structure, the second thing is the selector
-- the third is some sort of default, but I'm not sure how any syntax allows that
-- when using
-- nSelectLoc t s o
-- the t is the original term (or the parsed term)
-- because term here is a `nixParens` which is a Parser of ASTLoc
-- so there's also s, which is the annotated attribute path
-- then the o is a Maybe NExprLoc
-- so <$> will only apply it at a higher order function of a function
-- applied to the term
-- then it says <*> optional ... with selection dot, so that's the path selector
-- then there's optional or (so there's an or construct now)
-- `{ a = 123; }.b or 124`
-- never saw that syntax before, but it makes sense, would rather have a function for that instead though to simplify the number of constructs

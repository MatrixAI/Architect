{-# LANGUAGE OverloadedStrings #-}

module Architect.Compiler where

import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as PC
import qualified Text.Megaparsec.Char.Lexer as PL

-- we allow application
-- data Expr = App Expr Expr
--           | Var Var
--           | Lit Literal

-- data Var = Id Name Type

-- declaration syntax
-- thse are the top level declarations
-- we are declaring functions

-- Expression Declaration is a A = ...

-- DeclExpr is an expression declaration
-- should we be using names like that?



data Decl = ExprDecl BindGroup
data Name = Name String

-- underscores to prevent namespace issues
-- a bind group is needed for different bindings
-- but right now we just need a name
data BindGroup = BindGroup
  { _matchName  :: Name }




-- oh yea you can do Group 0
-- and Group 1
-- so we are allowing arbitrary functions?
-- but we just want to match on Automatons
-- wait if you can have pattern matching...

-- aren't we using Text?



-- data Match = Match
--   { _matchPat   :: [Pattern]
--   , _matchBody  :: Expr
--   , _matchGuard :: [Guard]
--   } deriving (Eq, Show)

-- data Pattern
--   = PVar Name              -- ^ x
--   | PCon Constr [Pattern]  -- ^ C x y
--   | PLit Literal           -- ^ 3
--   | PWild                  -- ^ _
--   deriving (Eq, Show)

-- data Literal = LitInt Int
--              | LitChar Char
--              | LitString [Word8]
--              deriving (Eq, Ord, Show)



-- we are using Void for now
-- later ParseError Char Void
-- means we have to use the same type
-- Note that the "token" tyep for STring and Text is Char
-- while for ByteStrings it's Word8
-- should we be using lazy Text later?
type Parser = P.Parsec Void T.Text

lineComment :: Parser ()
lineComment = PL.skipLineComment "--"

blockComment :: Parser ()
blockComment = PL.skipBlockComment "{-" "-}"

space :: Parser ()
space = PL.space PC.space1 lineComment blockComment

symbol :: T.Text -> Parser T.Text
symbol = PL.symbol space

-- the front end syntax is
-- we need ExprDecl BindGroup
declarations :: Parser Decl
declarations = undefined

-- parens :: Parser a -> Parser a
-- parens = P.between (symbol "(") (symbol ")")


-- we need to have a ADT
-- for this language
-- we need to have a higher order binding for it
-- how do we do that?
-- how do we allow data structures?
-- cvore language

-- binding :: Parser Binding


-- are we going to use # or // or --


-- we are going to try to parse the Automaton specification

{-

example syntax

A = Automaton {}

So we need bindings.

And we need constructors

We need to be parsing Text!

How does that work?

Note that we also have the ability to parse just these bindings

It says that with a delcaration we can have a certain bindings

-}


-- type List a = Fix (L a)

-- data L a b = Nil | Cons a b

-- -- the b is the actual contained element

-- instance Functor (L a) where
--   fmap f x = case x of
--     Nil -> Nil
--     Cons a b -> Cons a (f b)

-- length :: List a -> Int
-- length = cata $ \x -> case x of
--   Nil -> 0
--   Cons _ n -> n + 1

-- what you see here is that length which would normally be expressed in recursive manner, is now expressed in non recursive ways!?

-- using this also means that you can preserve a comon type
-- Bell next

-- data Toy b next

-- data Toy next = Output next
--               | Bell next
--               | Done

-- Output (Bell Done)

-- how does it know how to reduce the types

-- you are nesting the types
-- but if you are using Fix you end up not needing to fix the types!
-- using the Fix types you end up with a collapsed type
-- this seems kind of interesting, what is the use case?

-- grammars are uu

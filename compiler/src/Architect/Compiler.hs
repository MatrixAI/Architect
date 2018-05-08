{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Architect.Compiler where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void (Void)
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text, unpack)
import NeatInterpolation (text)

type Parser = Parsec Void String

data Notes = Item String | Items String [Notes]

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser String
pItem = lexeme (takeWhile1P Nothing f)
  where
    f x = isAlphaNum x || x == '-'


-- a single item
-- a single item that is only alphanum and -
-- no spaces allowed
-- but what if you were to allow spaces in between
-- like just until the newline?

-- wait we need lower level lexers and then convert them into an item
--


-- this parses a line fold
pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
  let ps = takeWhile1P Nothing f `sepBy1` try sc'
      f x = isAlphaNum x || x == '-'
  in unwords <$> ps <* sc

-- we go with a complex item
pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pLineFold)

-- this begins with an item list
pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome Nothing (return . (header, )) pComplexItem)

-- this parses an item list
parser = pItemList <* eof

testParser = do
  let source = unpack $ [text|
    first-chapter
      one
        abc abc
      two
        doo
        blah haha-blah
      three
        abc
          dee
           meh lol
           doo
  |]
  parseTest' parser source

-- what is the point of having such a line fold?

-- a block of indentation c is a sequence of tokens with indentation at least c
-- examples for a block is a where clause of haskell with no explicit braces
-- a line fold starting at line l and indentation c is a sequence of tokens that start at line l and possibly continue to subsequent lines as long as the indentation is greater than c

-- ok I get it, a line fold, starts at a certain point
-- and all subsequent stuff gets folded into  line
-- it's really like code folding
-- an example is like MIME headers
-- line folding based binding separation is used in Haskell as well

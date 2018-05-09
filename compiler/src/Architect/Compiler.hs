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

type Notes = [Note]
data Note = Note Item Notes deriving (Show)
type Item = String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing pred) lineComment empty
  where
    pred c = c == ' ' || c == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser Item
pItem = lexeme (takeWhile1P Nothing pred)
  where
    pred c = isAlphaNum c || c == '-'

pNote :: Parser Note
pNote = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return $ L.IndentMany Nothing (\notes -> return $ Note header notes) pNote

pNotes :: Parser Notes
pNotes = many pNote

testParser = do
  let source = unpack $ [text|
    heading1
      subheading1
      subheading2
        subheading2-1
    heading2
      subheading3
      subheading4
    # this is a comment
  |]
  parseTest' (pNotes <* eof) source

{-# LANGUAGE QuasiQuotes #-}

module NestedList where

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char                  (isAlphaNum)
import           Data.Text                  (unpack)
import           Data.Void                  (Void)
import           NeatInterpolation          (text)
import qualified Text.Megaparsec            as Parsec
import qualified Text.Megaparsec.Char       as CharParser
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec.Parsec Void String

type Item = String
data Note = Note Item Notes deriving (Show)
type Notes = [Note]

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "#"

spacesNewlines :: Parser ()
spacesNewlines = Lexer.space CharParser.space1 lineComment empty

spaces :: Parser ()
spaces = Lexer.space (void $ Parsec.takeWhile1P Nothing pred) lineComment empty
  where
    pred c = c == ' ' || c == '\t'

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

pItem :: Parser Item
pItem = lexeme (Parsec.takeWhile1P Nothing pred)
  where
    pred c = isAlphaNum c || c == '-'

pNote :: Parser Note
pNote = Lexer.indentBlock spacesNewlines p
  where
    p = do
      header <- pItem
      return $ Lexer.IndentMany Nothing (\notes -> return $ Note header notes) pNote

pNotes :: Parser Notes
pNotes = Parsec.many pNote

main :: IO ()
main = do
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
  Parsec.parseTest' (pNotes <* Parsec.eof) source
  print source

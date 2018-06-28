module Architect.Parser.Types where

import           Data.Void                  (Void)
import           Data.Text                  (Text)
import qualified Text.Megaparsec            as MP

-- our parser type has no error type for now
-- its main "tokens" type is Text
type Parser = MP.Parsec Void Text

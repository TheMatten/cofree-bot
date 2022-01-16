module CofreeBot.Parser (Parser, parse, module Exports) where

import Control.Monad.Combinators as Exports
import Control.Monad.Combinators.Expr as Exports
import Data.Bifunctor
import Data.Text (Text)
import Data.Void
import Text.Megaparsec qualified as M
import Text.Megaparsec as Exports
  ( label, hidden, try, lookAhead, notFollowedBy, withRecovery, observing, eof
  , takeWhileP, takeWhile1P, takeP, single, satisfy, anySingle, anySingleBut
  , oneOf, noneOf, chunk, (<?>), match, takeRest, atEnd
  )
import Text.Megaparsec.Char as Exports hiding (space, space1, hspace)
import Text.Megaparsec.Char.Lexer as Exports (decimal, scientific)

type Parser = M.Parsec Void Text

parse :: Parser a -> Text -> Either String a
parse p = first M.errorBundlePretty . M.parse p ""
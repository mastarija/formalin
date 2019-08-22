{- |
  This modules main function is 'parseLabel' which parses a single label into a
  workable format which can further be used to construct an 'Opaque' data
  structure.
-}
--
module Formalin.Parser where
--
import Prelude hiding ( takeWhile )
--
import Data.Text            ( Text )
import Data.List            ( notElem )
import Data.String          ( fromString )
import Control.Applicative  ( many, optional )
import Data.Attoparsec.Text ( Parser, char, string, choice, takeWhile, takeWhile1, endOfInput, parseOnly )
--

types :: [ Text ]
types = fmap fromString [ "Null", "Bool", "Binary", "Number", "String", "Vector" ]

valueType :: Parser Text
valueType = choice $ fmap string types

simple :: Char -> Bool
simple = flip notElem "[:]"

simpleLabel :: Parser Text
simpleLabel = takeWhile1 simple

nestedLabel :: Parser Text
nestedLabel = char '[' *> takeWhile simple <* char ']'

labelParser :: Parser ( [ Text ], Maybe Text )
labelParser = do
  simple <- simpleLabel
  nested <- many nestedLabel
  mcolon <- optional $ char ':'
  mvtype <- maybe ( pure Nothing ) ( const $ Just <$> valueType ) mcolon
  endOfInput >> pure ( simple : nested, mvtype )

parseLabel :: Text -> Either String ( [ Text ], Maybe Text )
parseLabel = parseOnly labelParser

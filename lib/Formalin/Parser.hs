{-# LANGUAGE OverloadedStrings #-}
--
module Formalin.Parser where
--
import Formalin.Model ( Segment (..) , FieldType (..) )
import Data.Attoparsec.Text ( Parser , many' , choice , digit , char , string , satisfy , inClass , takeWhile1 )
--

{- * Path -}

pathP :: Parser [ Segment ]
pathP = ( : ) <$> outerP <*> ( many' $ choice [ next1P , indexP , innerP ] )

next1P :: Parser Segment
next1P = Next1 <$ string "[]"

indexP :: Parser Segment
indexP = char '[' *> ( Index . read <$> ( ( : ) <$> satisfy ( inClass ['1'..'9'] ) <*> many' digit ) ) <* char ']'

innerP :: Parser Segment
innerP = char '[' *> ( Label <$> ( takeWhile1 $ ( /= ']' ) ) ) <* char ']'

outerP :: Parser Segment
outerP = Label <$> ( takeWhile1 $ ( /= '[' ) )

--

{- * Type -}

fieldTypeP :: Parser FieldType
fieldTypeP = choice [ nullP , boolP , jsonP , numberP , vectorP , stringP ]

nullP :: Parser FieldType
nullP = FNull <$ choice ( string <$> [ "NULL" , "Null" , "null" , "Z" , "z" ] )

boolP :: Parser FieldType
boolP = FBool <$ choice ( string <$> [ "BOOL" , "Bool" , "bool" , "B" , "b" ] )

jsonP :: Parser FieldType
jsonP = FJson <$ choice ( string <$> [ "JSON" , "Json" , "json" , "J" , "j" ] )

numberP :: Parser FieldType
numberP = FNumber <$ choice ( string <$> [ "Number" , "number" , "Num" , "num" , "N" , "n" ] )

vectorP :: Parser FieldType
vectorP = FVector
  <$  choice ( string <$> [ "Vector" , "vector" , "Vec" , "vec" , "V" , "v" ] )
  <*> choice [ char '[' *> fieldTypeP <* char ']' , pure FString ]

stringP :: Parser FieldType
stringP = FString <$ choice ( string <$> [ "S" , "s" , "Str" , "str" , "String" , "string" ] )

--

{- * Final -}

expressionP :: Parser ( [ Segment ] , FieldType )
expressionP = (,) <$> pathP <*> choice [ char ':' *> fieldTypeP , pure FString ]

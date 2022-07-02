{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Formalin.Model where
--
import GHC.Generics ( Generic )
import Numeric.Natural ( Natural )
import Data.Text ( Text , strip , unpack , toLower , splitOn )
import Data.Aeson ( Value (..) , decode )
import Data.Vector ( fromList )
import Text.Read ( readMaybe )
import Data.Text.Lazy ( fromStrict )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Bool ( bool )
--

type Path = [ Segment ]

data Segment
  = Next1
  | Index Natural
  | Label Text
  deriving ( Eq , Show , Generic )

data FieldType
  = FNull
  | FBool
  | FJson
  | FNumber
  | FString
  | FVector FieldType
  deriving ( Eq , Show , Generic )

--

caster :: FieldType -> Text -> Maybe Value
caster FNull _ = Just Null
caster FBool b = let lb = toLower b in bool Nothing ( Just $ Bool $ lb `elem` [ "true" , "1" ] ) ( not $ lb `elem` [ "true" , "false" , "0" , "1" ] )
caster FJson j = decode $ encodeUtf8 $ fromStrict j
caster FNumber n = Number <$> readMaybe ( unpack n )
caster FString s = Just $ String s
caster ( FVector t ) v = bool Nothing items ( t `elem` [ FNull , FBool , FNumber , FString ] )
  where
    items :: Maybe Value
    items =  Array . fromList <$> ( sequence $ caster t <$> ( strip <$> splitOn "," v ) )

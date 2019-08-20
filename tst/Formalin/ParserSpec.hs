{-# LANGUAGE OverloadedStrings #-}
--
module Formalin.ParserSpec where
--
import Prelude hiding ( concat )
--
import Data.Text ( Text, pack, concat )
import Data.String ( fromString )
import Formalin.Parser ( types, simple, parseLabel )
--
import Test.Hspec ( Spec, describe, it, shouldBe )
import Test.QuickCheck (  Arbitrary (..), NonNegative (..), property, elements, suchThat, listOf1 )
--

spec :: Spec
spec = do
  describe "parseLabel" $ do
    it "should parse complex labels" $ property $ \ s n t -> do
      parseLabel ( makeLabel s n t ) `shouldBe` Right ( makeResult s n t )

--

newtype SimpleLabel = SimpleLabel
  { getSimpleLabel :: Text
  } deriving ( Eq, Show )

instance Arbitrary SimpleLabel where
  arbitrary = SimpleLabel . pack <$> listOf1 arbitrary `suchThat` all simple

--

newtype NestedLabel = NestedLabel
  { getNestedLabel :: Text
  } deriving ( Eq, Show )

instance Arbitrary NestedLabel where
  arbitrary = NestedLabel . pack <$> arbitrary `suchThat` all simple

--

newtype LabelType = LabelType
  { getLabelType :: Text
  } deriving ( Eq, Show )

instance Arbitrary LabelType where
  arbitrary = LabelType <$> elements types

--

bracketer :: NestedLabel -> NestedLabel
bracketer ( NestedLabel t ) = NestedLabel $ "[" <> t <> "]"

makeLabel :: SimpleLabel -> [ NestedLabel ] -> Maybe LabelType -> Text
makeLabel ( SimpleLabel s ) nls mlt = concat
  [ s
  , concat $ fmap ( getNestedLabel . bracketer ) nls
  , maybe mempty ( \( LabelType t ) -> ":" <> t ) mlt
  ]

makeResult :: SimpleLabel -> [ NestedLabel ] -> Maybe LabelType -> ( [Text], Maybe Text )
makeResult ( SimpleLabel s ) nls mlt = ( s : fmap getNestedLabel nls, getLabelType <$> mlt )

module Formalin.ParserSpec ( spec ) where
--
import Test.Hspec
import Formalin.Parser
--

spec :: Spec
spec = do
  describe "stuff" $ do
    it "executes successfully" $ 1 `shouldBe` 1

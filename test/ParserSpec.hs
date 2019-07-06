module ParserSpec where

import Parser (clex)
import Test.Hspec
import Text.Printf (printf)

spec :: SpecWith ()
spec =
  describe "lexer" $ do
    it "drops whitespace" $ clex "  a\t\nb " `shouldBe` ["a", "b"]
    it "handles empty" $ clex "" `shouldBe` []
    it "vars" $ clex "a123 test_name" `shouldBe` ["a123", "test_name"]
    it "numbers" $ clex "123 0932" `shouldBe` ["123", "0932"]
    it "drops line comment" $ clex "a b -- c d \n e" `shouldBe` ["a", "b", "e"]
    it "single chars" $ do
      let ops = ["+", "*", "-", "/", "<", ">", "&", "|"]
      clex (concat ops) `shouldBe` ops
    it "double chars" $ do
      let ops = ["<=", "==", "/=", ">=", "->"]
      clex (concat ops) `shouldBe` ops

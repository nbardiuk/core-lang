module ParserSpec where

import Parser
  ( clex
  , keywords
  , pApply
  , pLit
  , pOneOrMore
  , pOneOrMoreWithSep
  , pVar
  , pZeroOrMore
  )
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "spec" $ do
    describe "lexer" $ do
      it "drops whitespace" $ clex "  a\t\nb " `shouldBe` ["a", "b"]
      it "handles empty" $ clex "" `shouldBe` []
      it "vars" $ clex "a123 test_name" `shouldBe` ["a123", "test_name"]
      it "numbers" $ clex "123 0932" `shouldBe` ["123", "0932"]
      it "drops line comment" $
        clex "a b -- c d \n e" `shouldBe` ["a", "b", "e"]
      it "single chars" $ do
        let ops = ["+", "*", "-", "/", "<", ">", "&", "|"]
        clex (concat ops) `shouldBe` ops
      it "double chars" $ do
        let ops = ["<=", "==", "/=", ">=", "->"]
        clex (concat ops) `shouldBe` ops
    describe "literal parser" $ do
      let p = pLit "hello"
      it "no tokens" $ p [] `shouldBe` []
      it "no match" $ p ["John", "!"] `shouldBe` []
      it "match" $
        p ["hello", "John", "!"] `shouldBe` [("hello", ["John", "!"])]
    describe "vars parser" $ do
      let p = pVar
      it "no tokens" $ p [] `shouldBe` []
      it "no match" $ p ["1", "213123"] `shouldBe` []
      it "keywords" $ p keywords `shouldBe` []
      it "match" $ p ["aa", "ab", "23"] `shouldBe` [("aa", ["ab", "23"])]
    describe "one or more" $ do
      let p = pOneOrMore (pLit "a")
      it "no tokens" $ p [] `shouldBe` []
      it "no match" $ p ["b", "c"] `shouldBe` []
      it "match" $
        p ["a", "a", "b"] `shouldBe` [(["a", "a"], ["b"]), (["a"], ["a", "b"])]
    describe "zero or more" $ do
      let p = pZeroOrMore (pLit "a")
      it "no tokens" $ p [] `shouldBe` [([], [])]
      it "no match" $ p ["b", "c"] `shouldBe` [([], ["b", "c"])]
      it "match" $
        p ["a", "a", "b"] `shouldBe`
        [(["a", "a"], ["b"]), (["a"], ["a", "b"]), ([], ["a", "a", "b"])]
    describe "apply" $ do
      let p = pZeroOrMore (pLit "a") `pApply` length
      it "no tokens" $ p [] `shouldBe` [(0, [])]
      it "no match" $ p ["b", "c"] `shouldBe` [(0, ["b", "c"])]
      it "match" $
        p ["a", "a", "b"] `shouldBe`
        [(2, ["b"]), (1, ["a", "b"]), (0, ["a", "a", "b"])]
    describe "one or more with separator" $ do
      let p = pOneOrMoreWithSep (pLit "a") (pLit ",")
      it "no tokens" $ p [] `shouldBe` []
      it "no match" $ p ["b", "c"] `shouldBe` []
      it "no separator" $ p ["a", "a", "b"] `shouldBe` [(["a"], ["a", "b"])]
      it "match" $
        p ["a", ",", "a", "b"] `shouldBe`
        [(["a", "a"], ["b"]), (["a"], [",", "a", "b"])]

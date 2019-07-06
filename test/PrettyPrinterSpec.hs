module PrettyPrinterSpec where

import Control.Monad (forM_)
import Data.List (intercalate)
import Language (Expr(..), nonRecursive, recursive)
import PrettyPrinter (pprint)
import Test.Hspec
import Text.Printf (printf)

spec :: SpecWith ()
spec =
  describe "prelude" $ do
    it "I" $ pprint [("I", ["x"], EVar "x")] `shouldBe` "I x = x"
    it "K" $ pprint [("K", ["x", "y"], EVar "x")] `shouldBe` "K x y = x"
    it "K1" $ pprint [("K1", ["x", "y"], EVar "y")] `shouldBe` "K1 x y = y"
    it "S" $
      pprint
        [ ( "S"
          , ["f", "g", "x"]
          , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
        ] `shouldBe`
      "S f g x = f x (g x)"
    it "compose" $
      pprint
        [ ( "compose"
          , ["f", "g", "x"]
          , EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
        ] `shouldBe`
      "compose f g x = f (g x)"
    it "twice" $
      pprint
        [("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))] `shouldBe`
      "twice f = compose f f"
    it "letrec expression" $
      pprint
        [ ( "call"
          , ["f"]
          , ELet recursive [("x", EVar "42")] (EAp (EVar "f") (EVar "x")))
        ] `shouldBe`
      intercalate
        "\n"
        ["call f = letrec", "           x = 42", "         in f x"]
    it "let expression" $
      pprint
        [ ( "call"
          , ["f"]
          , ELet nonRecursive [("x", EVar "42")] (EAp (EVar "f") (EVar "x")))
        ] `shouldBe`
      intercalate "\n" ["call f = let", "           x = 42", "         in f x"]
    it "case expression" $
      pprint
        [ ( "the_name"
          , ["x"]
          , ECase
              (EAp (EVar "f") (EVar "x"))
              [(1, ["a", "b"], EVar "42"), (2, ["c"], EVar "13")])
        ] `shouldBe`
      intercalate
        "\n"
        [ "the_name x = case f x of"
        , "               <1> a b -> 42"
        , "               <2> c -> 13"
        ]
    it "lambda expression" $
      pprint
        [ ( "call"
          , ["x"]
          , EAp (ELam ["a"] (EAp (EVar "f") (EVar "a"))) (EVar "x"))
        ] `shouldBe`
      "call x = (\\a -> f a) x"
    describe "infix operators" $
      forM_ ["+", "*", "-", "/", "<", "<=", "==", "/=", ">=", ">", "&", "|"] $ \op ->
        it op $
        pprint [("f", ["x", "y"], EAp (EAp (EVar op) (EVar "x")) (EVar "y"))] `shouldBe`
        printf "f x y = x %s y" op

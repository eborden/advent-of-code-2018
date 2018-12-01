#!/usr/bin/env stack
-- stack --resolver lts-12.20 script --package hspec-expectations
{-# LANGUAGE LambdaCase #-}
import           Data.Bifunctor          (bimap)
import           Test.Hspec.Expectations

main :: IO ()
main = do
  "+1, -2, +3, +1" `shouldEvalTo` 3
  "+1, +1, +1" `shouldEvalTo`  3
  "+1, +1, -2" `shouldEvalTo`  0
  "-1, -2, -3" `shouldEvalTo` (-6)

data Change
  = Add Int
  | Subtract Int
  | Unexpected Char

shouldEvalTo :: String -> Int -> Expectation
shouldEvalTo str i =
  eval (parseDisplay str) `shouldBe` i

eval :: [Change] -> Int
eval = \case
  [] -> 0
  Add i:rest -> i + eval rest
  Subtract i:rest -> eval rest - i
  Unexpected _:rest -> eval rest

parseDisplay :: String -> [Change]
parseDisplay = \case
  [] -> []
  ' ':rest -> parseDisplay rest
  '+':rest -> takeInt Add rest
  '-':rest -> takeInt Subtract rest
  char:rest -> Unexpected char : parseDisplay rest

takeInt :: (Int -> Change) -> String -> [Change]
takeInt f str = f int : parseDisplay rest
  where
    (int, rest) = bimap read (drop 1) $ span notComma str
    notComma = not . (== ',')

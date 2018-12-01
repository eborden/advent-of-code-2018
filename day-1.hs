#!/usr/bin/env stack
-- stack --resolver lts-12.20 script --package hspec-expectations --package containers
{-# LANGUAGE LambdaCase #-}
import           Data.Bifunctor          (bimap)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as Map
import           Data.List               (intercalate)
import           Data.Monoid             (First (First, getFirst))
import           Test.Hspec.Expectations (Expectation, shouldBe)

main :: IO ()
main = do
  "+1, -2, +3, +1" `shouldEvalTo` 3
  "+1, +1, +1" `shouldEvalTo`  3
  "+1, +1, -2" `shouldEvalTo`  0
  "-1, -2, -3" `shouldEvalTo` (-6)
  findDup "+1, -1" `shouldBe` Just 0
  findDup "+3, +3, +4, -2, -4" `shouldBe` Just 10
  findDup "-6, +3, +8, +5, -6" `shouldBe` Just 5
  findDup "+7, +7, -2, -7, -4" `shouldBe` Just 14
  input <- intercalate "," . lines <$> getContents
  print . eval $ parseDisplay input
  print $ findDup input

findDup :: String -> Maybe Int
findDup = getFirst . go (Map.singleton 0 1) 0 . cycle . parseDisplay
  where
    go bloom acc [] = First Nothing
    go bloom acc (x:xs) = case x of
      Add i        -> accumulate (acc + i) bloom xs
      Subtract i   -> accumulate (acc - i) bloom xs
      Unexpected _ -> go bloom acc xs
    accumulate i bloom xs = stayOrGo i bloom <> go (add1 i bloom) i xs
    add1 = Map.alter (Just . maybe 1 (+1))
    stayOrGo i bloom
      | Map.findWithDefault 0 i bloom == 1 = First $ Just i
      | otherwise = First Nothing

data Change
  = Add Int
  | Subtract Int
  | Unexpected Char

shouldEvalTo :: String -> Int -> Expectation
shouldEvalTo str i =
  eval (parseDisplay str) `shouldBe` i

eval :: [Change] -> Int
eval = foldr go 0
 where
  go = \case
    Add i -> (i +)
    Subtract i -> subtract i
    Unexpected _ -> id

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

import Test.QuickCheck
import Board
import Checker
{-# LANGUAGE TemplateHaskell #-}

main :: IO ()
main = do
      putStrLn "Test for lists"
      quickCheckWith (stdArgs {maxSuccess=1000}) ((\s -> (reverse.reverse) s == s) :: [Piece] -> Bool)

      putStrLn "Board after update has the same length of fields"
      quickCheckWith (stdArgs {maxSuccess=1000}) ((\b f1 f2 -> (length $ fields b) == (length $ fields $ updateFields b f1 f2)) :: Board -> Field -> Field -> Bool)

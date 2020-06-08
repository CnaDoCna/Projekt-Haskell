import Test.QuickCheck
import Board

main :: IO ()
main = do
      putStrLn "Test for lists"
      quickCheckWith (stdArgs {maxSuccess=10000}) ((\s -> (reverse.reverse) s == s) :: [Value] -> Bool)

      putStrLn "Check if board size is equal to number of board fields"
      quickCheckWith (stdArgs {maxSuccess=10000}) ((\b ->  ((size b)^2) == (length $ fields b)) :: Board -> Bool)
      -- chcemy sprawdzic czy funkcja Board łączy rozmiar z iloscia pól, bo mozna wpisac poki co Board 5 []

      putStrLn "Board after update has the same length of fields"
      quickCheckWith (stdArgs {maxSuccess=10000}) ((\b f -> (length $ fields b) == (length $ fields $ updateField b f)) :: Board -> Field -> Bool)

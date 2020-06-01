module Checker
    ( checkMove
    ) where

import Board
import Data.List.Split
import Debug.Trace (traceShowId)

checkMove :: Board -> Field -> Field -> Bool
checkMove b fn@(Field cn pn@(Piece _ t)) fo@(Field co _) =
    case t of
      King -> kChecker b cn co
  {-    Queen -> qChecker b cn co
       Rook -> rChecker b cn co
      Bishop -> bChecker b cn co
      Knight -> nChecker b cn co
      Pawn -> pChecker b cn co-}


-- jezeli ten sam pionek co sprawdzany stoi w miejscu dozwolonym do ruchu na starej planszy to jest to przesza pozycja pionka - musi zniknac
kChecker :: Board -> [Char] -> [Char] -> Bool
kChecker b@(Board fs) cn co@(coy:cox:[])
  | cn `elem` (traceShowId allowedFields) = True
  | otherwise = False
   where
     allowedFields = [[y, x] | y <- [pred coy..succ coy], x <- [pred cox..succ cox], [y, x] /= co, y < '9', y > '0', x < 'i']

{-
qChecker :: Board -> [Char] -> [Char] -> Bool
qChecker b@(Board fs) cn co@(coy:cox:[])
 | cn `elem` (traceShowId allowedFields) = True
 | otherwise = False
  where
    allowedFields = [[y, cox] | y <- ['1'..'8'], [y, x] /= co, y < '9', y > '0', x < 'i'] ++
                    [[coy, x] | y <- ['a'..'b'], [y, x] /= co, y < '9', y > '0', x < 'i'] ++
                    [[y, x] | (y, x) <- (zip ['a'..'b'], [y, x] /= co, y < '9', y > '0', x < 'i']-}

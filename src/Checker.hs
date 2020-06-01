module Checker
    ( checkMove
    ) where

import Board
import Data.List.Split
import Debug.Trace (traceShowId)

checkMove :: Board -> Field -> Field -> Bool
checkMove b fn@(Field cn pn@(Piece _ t)) fo@(Field co _) = --jest prawdziwe jesli
    case t of
      King -> kChecker b cn co
  {-    Queen -> qChecker b cn co
      Rook -> rChecker b cn co
      Bishop -> bChecker b cn co
      Knight -> nChecker b cn co
      Pawn -> pChecker b cn co
-}

-- jezeli ten sam pionek co sprawdzany stoi w miejscu dozwolonym do ruchu na starej planszy to jest to przesza pozycja pionka - musi zniknac
kChecker :: Board -> [Char] -> [Char] -> Bool
kChecker b@(Board fs) cn co@(coy:cox:[])
  | cn `elem` allowedFields = True
  | otherwise = False
   where
     allowedFields = [[y, x] | (y, x) <- (zip [pred coy..succ coy] [pred cox..succ cox]), [y, x] /= co]

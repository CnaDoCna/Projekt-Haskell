module Checker
    ( checkMove
    ) where

import Board
import Data.List.Split
import Debug.Trace (traceShowId)

checkMove :: Board -> Field -> Field -> Bool
checkMove _ fn@(Field _ Empty) _ = False
checkMove b fn@(Field cn (Piece k t)) fo@(Field co _) =
    case t of
      King -> kChecker b cn co
      Queen -> qChecker b cn co
      Rook -> rChecker b cn co
      Bishop -> bChecker b cn co
      Knight -> nChecker b cn co
      Pawn -> pChecker b cn co k


-- jezeli ten sam pionek co sprawdzany stoi w miejscu dozwolonym do ruchu na starej planszy to jest to przesza pozycja pionka - musi zniknac
kChecker :: Board -> [Char] -> [Char] -> Bool
kChecker b@(Board fs) cn co@[coy,cox]
  | cn `elem` (traceShowId allowedFields) = True
  | otherwise = False
   where
     allowedFields = [[y, x] | y <- [pred coy..succ coy], x <- [pred cox..succ cox], [y, x] /= co, y < '9', y > '0', x < 'i', x <= 'a']


qChecker :: Board -> [Char] -> [Char] -> Bool
qChecker b@(Board fs) cn co@[coy,cox]
 | cn `elem` (traceShowId allowedFields) = True
 | otherwise = False
  where
    allowedFields = [[y, cox] | y <- ['1'..'8'], [y, cox] /= co, y < '9', y > '0'] ++
                    [[coy, x] | x <- ['a'..'h'], [coy, x] /= co, x < 'i', x <= 'a'] ++
                    [[y, x] | (y, x) <- (zip [coy..] [cox..]), [y, x] /= co, y < '9', y > '0', x < 'i', x <= 'a'] ++
                    [[y, x] | (y, x) <- (zip [coy..] [cox..]), [y, x] /= co, y < '9', y > '0', x < 'i', x <= 'a'] ++
                    [[y, x] | (y, x) <- (zip [coy, pred coy..] [cox, pred cox..]), y < '9', y > '0', x < 'i', x <= 'a'] ++
                    [[y, x] | (y, x) <- (zip [coy, pred coy..] [cox..]), y < '9', y > '0', x < 'i', x <= 'a'] ++
                    [[y, x] | (y, x) <- (zip [coy..] [cox, pred cox..]), y < '9', y > '0', x < 'i', x <= 'a']


rChecker ::  Board -> [Char] -> [Char] -> Bool
rChecker b@(Board fs) cn co@[coy,cox]
 | cn `elem` (traceShowId allowedFields) = True
 | otherwise = False
  where
    allowedFields = [[y, cox] | y <- ['1'..'8'], [y, cox] /= co, y < '9', y > '0'] ++
                    [[coy, x] | x <- ['a'..'h'], [coy, x] /= co, x < 'i', x <= 'a']


bChecker :: Board -> [Char] -> [Char] -> Bool
bChecker b@(Board fs) cn co@[coy,cox]
 | cn `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = (traceShowId [[y, x] | (y, x) <- (zip [coy..] [cox..]), [y, x] /= co, y < '9', y > '0', x < 'i', x >= 'a']) ++
                    (traceShowId [[y, x] | (y, x) <- (zip [coy..] [cox..]), [y, x] /= co, y < '9', y > '0', x < 'i', x >= 'a']) ++
                    (traceShowId [[y, x] | (y, x) <- (zip [coy, pred coy..] [cox, pred cox..]), y < '9', y > '0', x < 'i', x >= 'a']) ++
                    (traceShowId [[y, x] | (y, x) <- (zip [coy, pred coy..] [cox..]), y < '9', y > '0', x < 'i', x >= 'a']) ++
                    (traceShowId [[y, x] | (y, x) <- (zip [coy..] [cox, pred cox..]), y < '9', y > '0', x < 'i', x >= 'a'])


nChecker :: Board -> [Char] -> [Char] -> Bool
nChecker b@(Board fs) cn co@[coy,cox]
 | cn `elem` (traceShowId allowedFields) = True
 | otherwise = False
  where
    allowedFields = [[y, x] | x <- [pred(pred cox),succ(succ cox)], y <- [pred coy,succ coy], y < '9', y > '0', x < 'i', x <= 'a'] ++
                    [[y, x] | x <- [pred cox,succ cox], y <- [pred(pred coy),succ(succ coy)], y < '9', y > '0', x < 'i', x <= 'a']


pChecker :: Board -> [Char] -> [Char] -> PColor -> Bool
pChecker b@(Board fs) cn co@[coy,cox] k
 | cn `elem` (traceShowId allowedFieldsW) && k == White = True
 | cn `elem` (traceShowId allowedFieldsB) && k == Black = True
 | otherwise = False
  where
    allowedFieldsW = [[y, cox] | y <- [pred coy], y < '9', y > '0']
    allowedFieldsB = [[y, cox] | y <- [succ coy], y < '9', y > '0']

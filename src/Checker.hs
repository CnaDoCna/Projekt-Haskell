module Checker
    ( checkMove
    ) where

import Board
import Data.List.Split
import Debug.Trace (traceShowId)

checkMove :: Board -> Field -> Field -> PColor -> Bool
checkMove _ ff@(Field _ Empty) _ _ = False
--checkMove b ff@(Field fc movedPiece@(Piece fk ft)) sf@(Field sc Empty) player = checkMove b ff@(Field fc movedPiece) sf@(Field sc movedPiece) player
checkMove b ff@(Field fc movedPiece@(Piece fk ft)) sf@(Field sc _) player =
  if (fk /= player) then False -- gracz nie moze przesunąc pionka przeciwnika ani postawic pionka na polu zajetym przez swoj pionek
  else
    case ft of
     King -> kingChecker b fc sc
     Queen -> queenChecker b fc sc
     Rook -> rookChecker b fc sc
     Bishop -> bishopChecker b fc sc
     Knight -> knightChecker b fc sc
     Pawn -> pawnChecker b fc sc fk


kingChecker :: Board -> [Char] ->  [Char] -> Bool
kingChecker b@(Board fs) fc@[fcy,fcx] sc
  | sc `elem` allowedFields  = True
  | otherwise = False
   where
     allowedFields = [[y, x] | y <- [pred fcy..succ fcy], x <- [pred fcx..succ fcx], [y, x] /= fc, y < '9', y > '0', x < 'i', x <= 'a']


queenChecker :: Board -> [Char] -> [Char] -> Bool
queenChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = (emptyPaths fs [[y, fcx] | y <- [fcy, pred fcy..'1'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPaths fs [[y, fcx] | y <- [fcy..'8'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPaths fs [[fcy, x] | x <- [fcx, pred fcx..'a'], [fcy, x] /= fc, x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[fcy, x] | x <- [fcx..'h'], [fcy, x] /= fc, x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy..] [fcx..]), [y, x] /= fc, y < '9', y > '0', x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx..]), y < '9', y > '0', x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x <= 'a'] [[]])


rookChecker ::  Board -> [Char] -> [Char] -> Bool
rookChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = (emptyPaths fs [[y, fcx] | y <- [fcy, pred fcy..'1'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPaths fs [[y, fcx] | y <- [fcy..'8'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPaths fs [[fcy, x] | x <- [fcx, pred fcx..'a'], [fcy, x] /= fc, x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[fcy, x] | x <- [fcx..'h'], [fcy, x] /= fc, x < 'i', x <= 'a'] [[]])


bishopChecker :: Board -> [Char] -> [Char] -> Bool
bishopChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy..] [fcx..]), [y, x] /= fc, y < '9', y > '0', x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx..]), y < '9', y > '0', x < 'i', x <= 'a'] [[]]) ++
                    (emptyPaths fs [[y, x] | (y, x) <- (zip [fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x <= 'a'] [[]])



knightChecker :: Board -> [Char] -> [Char] -> Bool
knightChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = [[y, x] | x <- [pred(pred fcx),succ(succ fcx)], y <- [pred fcy,succ fcy], y < '9', y > '0', x < 'i', x <= 'a'] ++
                    [[y, x] | x <- [pred fcx,succ fcx], y <- [pred(pred fcy),succ(succ fcy)], y < '9', y > '0', x < 'i', x <= 'a']


pawnChecker :: Board -> [Char] -> [Char] -> PColor -> Bool
pawnChecker b@(Board fs) fc@[fcy,fcx] sc k
 | sc `elem` allowedFieldsW && k == White = True
 | sc `elem` allowedFieldsB && k == Black = True
 | otherwise = False
  where
    allowedFieldsW = [[y, fcx] | y <- [pred fcy], y < '9', y > '0']
    allowedFieldsB = [[y, fcx] | y <- [succ fcy], y < '9', y > '0']



emptyPaths :: [Field] -> [[Char]] -> [[Char]] -> [[Char]]
emptyPaths fs [] new = new
emptyPaths fs (c:cs) new =
 if (whatPiece fs c) /= Empty then new
 else emptyPaths fs cs (c : new)

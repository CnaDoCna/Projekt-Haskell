module Checker
    ( checkMove
    ) where

import Board
import Data.List.Split
import Debug.Trace (traceShowId)

checkMove :: Board -> [Char] -> [Char] -> Piece -> Piece -> PColor -> Bool
checkMove b fc sc fp@(Piece fk ft) sp@(Piece sk st) player =
  if (fk /= player || sk == fk) then False -- gracz nie moze przesunąc pionka przeciwnika ani postawic pionka na polu zajetym przez swoj pionek
  else
    case ft of
     King -> kingChecker b fc sc
     Queen -> queenChecker b fc sc
     Rook -> rookChecker b fc sc
     Bishop -> bishopChecker b fc sc
     Knight -> knightChecker b fc sc
     Pawn -> pawnChecker b fc sc fk
     _ -> False



kingChecker :: Board -> [Char] ->  [Char] -> Bool
kingChecker b@(Board fs) fc@[fcy,fcx] sc
  | sc `elem` allowedFields  = True
  | otherwise = False
   where
     allowedFields = [[y, x] | y <- [pred fcy..succ fcy], x <- [pred fcx..succ fcx], [y, x] /= fc, y < '9', y > '0', x < 'i', x >= 'a']


queenChecker :: Board -> [Char] -> [Char] -> Bool
queenChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = (emptyPath fs [[y, fcx] | y <- [fcy, pred fcy..'1'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPath fs [[y, fcx] | y <- [fcy..'8'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPath fs [[fcy, x] | x <- [fcx, pred fcx..'a'], [fcy, x] /= fc, x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[fcy, x] | x <- [fcx..'h'], [fcy, x] /= fc, x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx..]), [y, x] /= fc, y < '9', y > '0', x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx..]), y < '9', y > '0', x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x >= 'a'] [[]])


rookChecker ::  Board -> [Char] -> [Char] -> Bool
rookChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = (emptyPath fs [[y, fcx] | y <- [fcy, pred fcy..'1'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPath fs [[y, fcx] | y <- [fcy..'8'], [y, fcx] /= fc, y < '9', y > '0'] [[]]) ++
                    (emptyPath fs [[fcy, x] | x <- [fcx, pred fcx..'a'], [fcy, x] /= fc, x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[fcy, x] | x <- [fcx..'h'], [fcy, x] /= fc, x < 'i', x >= 'a'] [[]])


bishopChecker :: Board -> [Char] -> [Char] -> Bool
bishopChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx..]), [y, x] /= fc, y < '9', y > '0', x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx..]), y < '9', y > '0', x < 'i', x >= 'a'] [[]]) ++
                    (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx, pred fcx..]), y < '9', y > '0', x < 'i', x >= 'a'] [[]])



knightChecker :: Board -> [Char] -> [Char] -> Bool
knightChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
  where
    allowedFields = [[y, x] | x <- [pred(pred fcx),succ(succ fcx)], y <- [pred fcy,succ fcy], y < '9', y > '0', x < 'i', x >= 'a'] ++
                    [[y, x] | x <- [pred fcx,succ fcx], y <- [pred(pred fcy),succ(succ fcy)], y < '9', y > '0', x < 'i', x >= 'a']


pawnChecker :: Board -> [Char] -> [Char] -> PColor -> Bool
pawnChecker b@(Board fs) fc@[fcy,fcx] sc k =
  case k of
    White -> if sc `elem` allowedFieldsW then True else False
    Black -> if sc `elem` allowedFieldsB then True else False
  where
    allowedFieldsW = (emptyPath fs [[y, fcx] | y <- [pred fcy], y > '0',  y < '9'] [[]]) ++
                     (opponentsPiece fs [[y, x] | y <- [pred fcy], x <- [succ fcx], y < '9', y > '0', x < 'i', x >= 'a'] Black) ++
                     (opponentsPiece fs [[y, x] | y <- [pred fcy], x <- [pred fcx], y < '9', y > '0', x < 'i', x >= 'a'] Black)
    allowedFieldsB = (emptyPath fs [[y, fcx] | y <- [succ fcy], y < '9', y > '0'] [[]]) ++
                     (opponentsPiece fs [[y, x] | y <- [succ fcy], x <- [succ fcx], y > '0', y < '9', x < 'i', x >= 'a'] White) ++
                     (opponentsPiece fs [[y, x] | y <- [succ fcy], x <- [pred fcx], y > '0', y < '9', x < 'i', x >= 'a'] White)


--dla queen rook i bishop sprawdza czy dozwolone pola sa takze puste - nie moga przeskakiwac innych pionkow
emptyPath :: [Field] -> [[Char]] -> [[Char]] -> [[Char]]
emptyPath fs [] new = new
emptyPath fs (c:cs) new =
 if (whatPiece fs c) /= (Piece NoColor NoType) then (c : new)
 else emptyPath fs cs (c : new)

--dla pawn sprawdza czy pola naprzeciw/obok zawierają pionek przeciwnika do zbicia
opponentsPiece :: [Field] -> [[Char]] -> PColor -> [[Char]]
opponentsPiece fs [] k = []
opponentsPiece fs (c:[]) k =
  if colorof (whatPiece fs c) == k then [c] else []

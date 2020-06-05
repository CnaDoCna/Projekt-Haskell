module Checker
    ( checkMove
    ) where

import Board
import Data.List.Split

checkMove :: Board -> [Char] -> [Char] -> Piece -> Piece -> PColor -> Bool
checkMove b fc sc fp@(Piece fk ft) sp@(Piece sk st) player =
  if (fk /= player || sk == fk || sc == fc) then False -- gracz nie moze przesunąc pionka przeciwnika ani postawic pionka na polu zajetym przez swoj pionek
  else
    case ft of
     King -> kingChecker b fc sc
     Queen -> queenChecker b fc sc
     Rook -> rookChecker b fc sc
     Bishop -> bishopChecker b fc sc
     Knight -> knightChecker b fc sc
     Pawn -> pawnChecker b fc sc player
     _ -> False



kingChecker :: Board -> [Char] ->  [Char] -> Bool
kingChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields  = True
 | otherwise = False
  where
    allowedFields = [[y, x] | y <- [pred fcy..succ fcy], x <- [pred fcx..succ fcx], [y, x] /= fc, y <= '8', y >= '1', x <= 'h', x >= 'a']


queenChecker :: Board -> [Char] -> [Char] -> Bool
queenChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
 where
   allowedFields = (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]]) ++
                   (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx, pred fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]]) ++
                   (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]]) ++
                   (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx, pred fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]]) ++
                   (emptyPath fs [[y, fcx] | y <- [fcy, pred fcy..'1'], y <= '8', y >= '1', [y, fcx] /= fc] [[]]) ++
                   (emptyPath fs [[y, fcx] | y <- [fcy..'8'], y <= '8', y >= '1', [y, fcx] /= fc] [[]]) ++
                   (emptyPath fs [[fcy, x] | x <- [fcx, pred fcx..'a'], x <= 'h', x >= 'a', [fcy, x] /= fc] [[]]) ++
                   (emptyPath fs [[fcy, x] | x <- [fcx..'h'], x <= 'h', x >= 'a', [fcy, x] /= fc] [[]])


rookChecker ::  Board -> [Char] -> [Char] -> Bool
rookChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
 where
   allowedFields = (emptyPath fs [[y, fcx] | y <- [fcy, pred fcy..'1'], y <= '8', y >= '1', [y, fcx] /= fc] [[]]) ++
                   (emptyPath fs [[y, fcx] | y <- [fcy..'8'], y <= '8', y >= '1', [y, fcx] /= fc] [[]]) ++
                   (emptyPath fs [[fcy, x] | x <- [fcx, pred fcx..'a'], x <= 'h', x >= 'a', [fcy, x] /= fc] [[]]) ++
                   (emptyPath fs [[fcy, x] | x <- [fcx..'h'], x <= 'h', x >= 'a', [fcy, x] /= fc] [[]])


bishopChecker :: Board -> [Char] -> [Char] -> Bool
bishopChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
 where
   allowedFields = (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]]) ++
                   (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx, pred fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]]) ++
                   (emptyPath fs [[y, x] | (y, x) <- (zip [fcy, pred fcy..] [fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]]) ++
                   (emptyPath fs [[y, x] | (y, x) <- (zip [fcy..] [fcx, pred fcx..]), y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] [[]])



knightChecker :: Board -> [Char] -> [Char] -> Bool
knightChecker b@(Board fs) fc@[fcy,fcx] sc
 | sc `elem` allowedFields = True
 | otherwise = False
 where
   allowedFields = [[y, x] | x <- [pred(pred fcx),succ(succ fcx)], y <- [pred fcy,succ fcy], y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] ++
                   [[y, x] | x <- [pred fcx,succ fcx], y <- [pred(pred fcy),succ(succ fcy)], y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc]


pawnChecker :: Board -> [Char] -> [Char] -> PColor -> Bool
pawnChecker b@(Board fs) fc@[fcy,fcx] sc k =
 case k of
   White -> if sc `elem` allowedFieldsW then True else False
   Black -> if sc `elem` allowedFieldsB then True else False
   _ -> False
 where
   allowedFieldsW = (emptyField fs [[y, fcx] | y <- [pred fcy], y >= '1',  y <= '8', [y, fcx] /= fc]) ++
                    (opponentsPiece fs [[y, x] | y <- [pred fcy], x <- [succ fcx], y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] k) ++
                    (opponentsPiece fs [[y, x] | y <- [pred fcy], x <- [pred fcx], y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] k)
   allowedFieldsB = (emptyField fs [[y, fcx] | y <- [succ fcy], y <= '8', y >= '1', [y, fcx] /= fc]) ++
                    (opponentsPiece fs [[y, x] | y <- [succ fcy], x <- [succ fcx], y >= '1', y <= '8', x <= 'h', x >= 'a', [y, x] /= fc] k) ++
                    (opponentsPiece fs [[y, x] | y <- [succ fcy], x <- [pred fcx], y >= '1', y <= '8', x <= 'h', x >= 'a', [y, x] /= fc] k)


--dla queen rook i bishop sprawdza czy dozwolone pola sa takze puste - plus kończą się jednym pionkiem
emptyPath :: [Field] -> [[Char]] -> [[Char]] -> [[Char]]
emptyPath fs [] new = new
emptyPath fs (c:cs) new =
 if (whatPiece fs c) /= (Piece NoColor NoType) then (c : new) else emptyPath fs cs (c : new)
  where

--dla pawn sprawdza czy pola zawierają pionek przeciwnika do zbicia
opponentsPiece :: [Field] -> [[Char]] -> PColor -> [[Char]]
opponentsPiece fs [] k = []
opponentsPiece fs (c:cs) k =
  if colorof (whatPiece fs c) == (otherPlayer k) then (c:cs) else cs


emptyField :: [Field] -> [[Char]] -> [[Char]]
emptyField fs [] = []
emptyField fs (c:cs) =
  if (whatPiece fs c) == (Piece NoColor NoType) then (c:cs) else cs

{-|
Module      : Checker
Description : Implements chess rules.
Copyright   : (c) Zuzanna Boruczkowska, 2020
              Marta Koczerska, 2020
              Małgorzata Orłowska, 2020
License      : GPL-3
Maintainer   : zuzbor5@st.amu.edu.pl
Stability    : stable
Portability  : POSTX
-}

module Checker
    ( checkMove
    , kingChecker
    , queenChecker
    , rookChecker
    , bishopChecker
    , knightChecker
    , pawnChecker
    , emptyPath
    , emptyFields
    , opponentsPiece
    , allowDoubleMove
    ) where

import Board
import Data.List.Split

-- | [@checkMove@] Takes a board, starting field coordinates, target field coordinates,
-- current starting field Piece, current target field Piece and current players' color.
--
-- Returns False when:
--
--     * color of starting Piece is different from players' (player can't move opponents' piece)
--     * color of starting Piece is the same as target Piece (player can't capture own piece)
--     * starting coordinates are the same as target coordinates (player can't stand in a place in a loop)
--     * type of the staring Piece is other then available (Piece is non existing)
--
-- | Returns True when Piece types' legality checkers returns True.
checkMove :: Board -> [Char] -> [Char] -> Piece -> Piece -> PColor -> Bool
checkMove b fc sc fp@(Piece fk ft) sp@(Piece sk st) player =
  if (fk /= player || sk == fk || sc == fc) then False
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
pawnChecker b@(Board fs) fc@[fcy,fcx] sc o =
 case o of
   White -> if sc `elem` allowedFieldsW then True else False
   Black -> if sc `elem` allowedFieldsB then True else False
   _ -> False
 where
   allowedFieldsW = (emptyFields fs [[y, fcx] | y <- [pred fcy], y >= '1',  y <= '8', [y, fcx] /= fc] [[]]) ++
                    (opponentsPiece fs [[y, x] | y <- [pred fcy], x <- [succ fcx], y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] o) ++
                    (opponentsPiece fs [[y, x] | y <- [pred fcy], x <- [pred fcx], y <= '8', y >= '1', x <= 'h', x >= 'a', [y, x] /= fc] o) ++
                    (emptyFields fs (allowDoubleMove [[y, fcx] | y <- [pred fcy, pred(pred fcy)], y <= '8', y >= '1', [y, fcx] /= fc] fcy o) [[]])
   allowedFieldsB = (emptyFields fs [[y, fcx] | y <- [succ fcy], y <= '8', y >= '1', [y, fcx] /= fc] [[]]) ++
                    (opponentsPiece fs [[y, x] | y <- [succ fcy], x <- [succ fcx], y >= '1', y <= '8', x <= 'h', x >= 'a', [y, x] /= fc] o) ++
                    (opponentsPiece fs [[y, x] | y <- [succ fcy], x <- [pred fcx], y >= '1', y <= '8', x <= 'h', x >= 'a', [y, x] /= fc] o) ++
                    (emptyFields fs (allowDoubleMove [[y, fcx] | y <- [succ fcy, succ(succ fcy)], y <= '8', y >= '1', [y, fcx] /= fc] fcy o) [[]])


-- | [@emptyPath@] Out of a list of legal moves' coordinates, creates a path of unoccupied coordines,
-- starting from the starting coordinate to a first encountered occupied coordinate.
-- Adds the coordinate to the list, as it can be occupied by a Piece to capture.
--
-- Used by Queen, Rook and Bishop Piece type.
--
-- >>> emptyPath ["6b", "6c", "6d", "6e", "6f"]
-- ["6b", "6c", "6d"]
emptyPath :: [Field] -> [[Char]] -> [[Char]] -> [[Char]]
emptyPath _ [] new = new
emptyPath fs (c:cs) new =
 if (whatPiece fs c) /= (Piece NoColor NoType) then (c : new) else emptyPath fs cs (c : new)



-- | [@opponentsPiece@] Out of a list of moves' coordinates,
-- checks if coordinates can exist as legal,
-- by being occupied by an opponents' piece.
--
-- Used by Pawn Piece type.
opponentsPiece :: [Field] -> [[Char]] -> PColor -> [[Char]]
opponentsPiece _ [] _ = []
opponentsPiece fs (c:cs) o =
  if colorof (whatPiece fs c) == (otherPlayer o) then (c:cs) else cs

-- | [@emptyField@] Out of a list of moves' coordinates,
-- checks if coordinates can exist as legal, by being unoccupied.
--
-- Used by Pawn Piece type.
emptyFields :: [Field] -> [[Char]] -> [[Char]] -> [[Char]]
emptyFields _ [] new = new
emptyFields fs (c:cs) new =
  if (whatPiece fs c) /= (Piece NoColor NoType) then new else emptyFields fs cs (c : new)

-- | [@allowDoubleMove@] Returns list of coordinates if the pawn Piece
-- stands on a starting position.
--
-- Used by Pawn Piece type.
allowDoubleMove :: [[Char]] -> Char -> PColor -> [[Char]]
allowDoubleMove [] _ _ = []
allowDoubleMove cs fcy o
 | fcy == '7' && o == White = cs
 | fcy == '2' && o == Black = cs
 | otherwise = []

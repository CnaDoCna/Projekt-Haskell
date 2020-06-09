module MateChecker
    (checkMate
    , opponentKing
    ) where

import Board
import Checker

checkMate b@(Board fs) c p@(Piece k t) =
  case t of
     King -> kingChecker b c (opponentKing fs k)
     Queen -> queenChecker b c (opponentKing fs k)
     Rook -> rookChecker b c (opponentKing fs k)
     Bishop -> bishopChecker b c (opponentKing fs k)
     Knight -> knightChecker b c (opponentKing fs k)
     Pawn -> pawnChecker b c (opponentKing fs k) k
     _ -> False

opponentKing :: [Field] -> PColor -> [Char]
opponentKing (f:fs) k
  | typeof (piece f) == King && colorof (piece f) == (otherPlayer k) = coords f
  | otherwise = opponentKing fs k
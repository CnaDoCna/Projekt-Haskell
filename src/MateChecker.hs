{-|
Module      : MateChecker
Description : Implements search for opponents' King to put in check.
Copyright   : (c) Zuzanna Boruczkowska, 2020
              Marta Koczerska, 2020
              Małgorzata Orłowska, 2020
License      : GPL-3
Maintainer   : zuzbor5@st.amu.edu.pl
Stability    : stable
Portability  : POSTX
-}

module MateChecker
    (checkMate
    , opponentKing
    ) where

import Board
import Checker

checkMate b@(Board fs) c p@(Piece o t) =
  case t of
     King -> kingChecker b c (opponentKing fs o)
     Queen -> queenChecker b c (opponentKing fs o)
     Rook -> rookChecker b c (opponentKing fs o)
     Bishop -> bishopChecker b c (opponentKing fs o)
     Knight -> knightChecker b c (opponentKing fs o)
     Pawn -> pawnChecker b c (opponentKing fs o) o
     _ -> False

opponentKing :: [Field] -> PColor -> [Char]
opponentKing (f:fs) o
  | typeof (piece f) == King && colorof (piece f) == (otherPlayer o) = coords f
  | otherwise = opponentKing fs o

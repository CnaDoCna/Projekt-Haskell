module Capturer
    ( checkCapture
    ) where

import Board
import Debug.Trace (traceShowId)

checkCapture :: Board -> Piece -> Piece -> [Char] -> [Char] -> PColor -> Bool
checkCapture _ _ (Piece NoColor NoType) _ _ _ = False
checkCapture b fp@(Piece fk ft) sp@(Piece sk tk) capturesW capturesB player
    | ft == Pawn = pawnCapturer
    | otherwise = pieceCapturer

pieceCapturer :: Bool
pieceCapturer = False

pawnCapturer :: Bool
pawnCapturer = False

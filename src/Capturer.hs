module Capturer
    ( checkCapture
    ) where

import Board
import Debug.Trace (traceShowId)

checkCapture :: Board -> Piece -> Piece -> [Char] -> [Char] -> Bool
checkCapture _ _ Empty _ _ = False
checkCapture b fp@(Piece fk ft) sp@(Piece sk tk) capturesWhite capturesBlack
    | ft == Pawn = pawnCapturer
    | otherwise = pieceCapturer

pieceCapturer :: Bool
pieceCapturer = False

pawnCapturer :: Bool
pawnCapturer = False

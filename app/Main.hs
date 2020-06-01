module Main where

import Board
import Checker
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    putStrLn "Chess Game\n"
    let board = initialBoard
        in move board

move :: Board -> IO ()
move board@(Board fs) = do
    putStrLn $ drawBoard board
    putStrLn "\nTake a piece from:"
    pastRaw <- getLine
    putStrLn "Place a piece on:"
    presentRaw <- getLine
    let present = read presentRaw :: [Char]
        past = read pastRaw :: [Char]
        fieldNew = Field present (movedPieceType fs past)
        fieldOld = Field past Empty
    if checkMove board fieldNew fieldOld == False
      then do
           putStrLn "Move forbidden"
           move board
    else
       let boardNew = updateField board fieldNew fieldOld
           in move boardNew

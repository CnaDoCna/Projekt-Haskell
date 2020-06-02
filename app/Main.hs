module Main where

import Board
import Checker
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    putStrLn "\nChess Game\n\nTo enter a move, type <DL>, \nwhere D is a digit coordinate \nand L is a letter coordinate.\n To quit the game type <q> anytime.\n"
    let board = initialBoard
        player = "White"
        in move board player

move :: Board -> [Char] -> IO ()
move board@(Board fs) player = do
    putStrLn $ drawBoard board
    putStrLn ("\n" ++ player ++ " pieces' turn.")
    putStrLn "Take a piece from:"
    pastRaw <- getLine
    putStrLn "Place a piece on:"
    presentRaw <- getLine
    let present = presentRaw
        past = pastRaw
        fieldNew = Field present (movedPiece fs past)
        fieldOld = Field past Empty
    if present == "q"
      then return()
    else
        if checkMove board fieldNew fieldOld == False
          then do
               putStrLn "\nMove forbidden. Try again!\n"
               move board player
        else
           let boardNew = updateField board fieldNew fieldOld
               in move boardNew (nextPlayer player)

nextPlayer :: [Char] -> [Char]
nextPlayer current = if current == "White" then "Black" else "White"

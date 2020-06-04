module Main where

import Board
import Checker
--import Capturer
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    putStrLn "\n\nChess Game\n\nTo enter a move, type <DL>, \nwhere D is a digit coordinate \nand L is a letter coordinate. \nTo quit the game type <q> anytime.\n\nUppercase = Whites \nLowercase = Blacks \n\n"
    let board = initialBoard
        player = White
        in move board player

move :: Board -> PColor -> IO ()
move board@(Board fs) player = do
    putStrLn $ drawBoard board
    putStrLn ("\n" ++ (show player) ++ " pieces' turn.")
    putStrLn "Take a piece from:"
    fstPosition <- getLine
    if fstPosition == "q"
      then return()
    else do
      putStrLn "Place a piece on:"
      sndPosition <- getLine

      let fstPiece = whatPiece fs fstPosition
          sndPiece = whatPiece fs sndPosition
      if checkMove board fstPosition sndPosition fstPiece sndPiece player == False
        then do
             putStrLn "\nForbidden move, forbidden piece or wrong input. \nTry again!\n"
             move board player
      else do
         if sndPiece /= Piece NoColor NoType
             then putStrLn ("\nMove successful!\n" ++
             (show $ colorof fstPiece) ++ " " ++ (show $ typeof fstPiece) ++
             " captures " ++ (show $ colorof sndPiece) ++ " " ++ (show $ typeof sndPiece) ++ "!\n")
         else putStrLn ("\nMove successful!\n")

         let newFstField = Field fstPosition (Piece NoColor NoType) -- pole z ktorego zostal zabrany pionek - zwracam jego nową zawartosc
             newSndField = Field sndPosition fstPiece -- pole na którym zostal postawiony pionek na przeszłej tablicy - zwracam jego nową zawartość
             boardNew = updateFields board newFstField newSndField
             in move boardNew (nextPlayer player)

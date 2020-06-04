module Main where

import Board
import Checker
import Capturer
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    putStrLn "\n\nChess Game\n\nTo enter a move, type <DL>, \nwhere D is a digit coordinate \nand L is a letter coordinate. \nTo quit the game type <q> anytime.\n\nUppercase = Whites \nLowercase = Blacks \n\n"
    let board = initialBoard
        player = White
        capturesWhite = []
        capturesBlack = []
        in move board player capturesWhite capturesBlack

move :: Board -> PColor -> [Char] -> [Char]-> IO ()
move board@(Board fs) player capturesWhite capturesBlack = do
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
          newFstField = Field fstPosition Empty -- pole z ktorego zostal zabrany pionek - zwracam jego nową zawartosc -empty
          newSndField = Field sndPosition fstPiece -- pole na którym zostal postawiony pionek na przeszłej tablicy - zwracam jego nową zawartość
      if checkMove board fstPiece sndPiece fstPosition sndPosition player == False
        then do
             putStrLn "\nForbidden move, forbidden piece or wrong input. \nTry again!\n"
             move board player capturesWhite capturesBlack
      else do
         if checkCapture board fstPiece sndPiece capturesWhite capturesBlack == True
             then putStrLn ("\nMove successful!\n" ++ (show fstPiece) ++ " captures " ++ (show sndPiece) ++ "!\n")
         else putStrLn ("\nMove successful!\n")

         let boardNew = updateFields board newFstField newSndField
             in move boardNew (nextPlayer player) capturesWhite capturesBlack

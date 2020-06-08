
module Main
 where

import Board
import Checker
import Data.List.Split (chunksOf)


main :: IO ()
main = do
    putStrLn "\n\nChess Game\n\nTo enter a move, type <DLDL>, \nwhere D is a digit coordinate, \nL is a letter coordinate\nand first is starting DL, second is target DL. \nTo quit the game type <q> anytime.\n\nUppercase = White \nLowercase = Black \n\n"
    let board = initialBoard
        player = White
        in move board player


move :: Board -> PColor -> IO ()
move board@(Board fs) player = do
    putStrLn $ drawBoard board
    putStrLn ("\n" ++ (show player) ++ " pieces' turn.")
    putStrLn "Make a move: "
    input <- getLine
    if input == "q"
      then return()
    else do
      let fstPosition = head $ chunksOf 2 input
          sndPosition = last $ chunksOf 2 input
          fstPiece = whatPiece fs fstPosition
          sndPiece = whatPiece fs sndPosition
      if checkMove board fstPosition sndPosition fstPiece sndPiece player == False
        then do
             putStrLn "\nForbidden move, forbidden piece or wrong input. \nTry again!\n"
             move board player
      else do
         if sndPiece /= Piece NoColor NoType
             then putStrLn ("\nMove successful!\n" ++
             (show $ colorof fstPiece) ++ " " ++ (show $ typeof fstPiece) ++ " captures " ++ (show $ colorof sndPiece) ++ " " ++ (show $ typeof sndPiece) ++ "!\n")
         else putStrLn ("\nMove successful!\n")

         let newFstField = Field fstPosition (Piece NoColor NoType)
             newSndField = Field sndPosition fstPiece
             boardNew = updateFields board newFstField newSndField
             in move boardNew (otherPlayer player)

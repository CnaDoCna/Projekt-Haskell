
module Main
 where

import Board
import Checker
import MateChecker
import Data.List.Split (chunksOf)
import Data.Char


main :: IO ()
main = do
    putStrLn "\n\n Chess Game\n\nTo enter a move, type <DLDL>, \nwhere D is a digit coordinate, \nL is a letter coordinate \nand first is starting DL, second is target DL. \nTo quit the game anytime press Enter without typing. \n\nUppercase = White \nLowercase = Black \n\n"
    let board = initialBoard
        player = White
        in move board player


move :: Board -> PColor -> IO ()
move board@(Board fs) player = do
    putStrLn $ drawBoard board
    putStrLn ("\n" ++ (map toUpper $ show player) ++ " pieces' turn.")
    putStrLn "Make a move: "
    input <- getLine
    if input == ""
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
       let newFstField = Field fstPosition (Piece NoColor NoType)
           newSndField = Field sndPosition fstPiece
           boardNew = updateFields board newFstField newSndField

       if sndPiece == Piece (otherPlayer player) King
           then
             putStrLn ("\n\nCHECKMATE!\n\n" ++ (show player) ++ " pieces' won!")

       else do
            if sndPiece /= Piece NoColor NoType
               then putStrLn ("\n\nMove successful!\n " ++
                    (show $ colorof fstPiece) ++ " " ++ (show $ typeof fstPiece) ++ " captures " ++ (show $ colorof sndPiece) ++ " " ++ (show $ typeof sndPiece) ++ "!\n")

            else if checkMate boardNew sndPosition fstPiece == True
                   then putStrLn ("\n\n" ++ (show $ otherPlayer player) ++ " King is IN CHECK!\n\n")
                else putStrLn ("\n\nMove successful!\n")

            move boardNew (otherPlayer player)

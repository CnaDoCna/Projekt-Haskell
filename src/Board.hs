{-|
Module      : Board
Description : Implements actions, layout and content of the chess board.
Copyright   : (c) Zuzanna Boruczkowska, 2020
              Marta Koczerska, 2020
              Małgorzata Orłowska, 2020
License      : GPL-3
Maintainer   : zuzbor5@st.amu.edu.pl
Stability    : stable
Portability  : POSTX
-}

module Board
    ( Piece (..)
    , Field (..)
    , Board (..)
    , PType (..)
    , PColor (..)
    , drawBoard
    , initialBoard
    , updateFields
    , sortFields
    , whatPiece
    , otherPlayer
    ) where

import Data.List       (sortBy, intercalate)
import Data.List.Split (chunksOf)
import Test.QuickCheck
import Data.Functor

data PColor = White | Black | NoColor
   deriving(Show, Eq)
data PType = King | Queen | Rook | Bishop | Knight | Pawn | NoType
   deriving(Show, Eq)
-- | Piece NoColor NoType is a Void Piece. I needed it to have color and type so it can match in functions' pattermatching : ).
data Piece = Piece { colorof :: PColor, typeof :: PType}
  deriving(Eq)

data Field = Field { coords :: [Char], piece :: Piece }

data Board = Board { fields :: [Field] }


instance Show Piece where
    show p = case p of
        Piece White King -> "K"
        Piece White Queen -> "Q"
        Piece White Rook -> "R"
        Piece White Bishop -> "B"
        Piece White Knight -> "N"
        Piece White Pawn -> "P"
        Piece Black King -> "k"
        Piece Black Queen -> "q"
        Piece Black Rook -> "r"
        Piece Black Bishop -> "b"
        Piece Black Knight -> "n"
        Piece Black Pawn -> "p"
        _ -> " "



initialBoard :: Board
initialBoard = Board (map (\(c,p) -> Field c p) $ zip boardCoords initPositions)
          where initPositions = [Piece Black Rook, Piece Black Knight, Piece Black Bishop, Piece Black Queen, Piece Black King, Piece Black Bishop, Piece Black Knight, Piece Black Rook,
                                 Piece Black Pawn, Piece Black Pawn, Piece Black Pawn, Piece Black Pawn, Piece Black Pawn, Piece Black Pawn, Piece Black Pawn, Piece Black Pawn,
                                 Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType,
                                 Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType,
                                 Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType,
                                 Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType, Piece NoColor NoType,
                                 Piece White Pawn, Piece White Pawn, Piece White Pawn, Piece White Pawn, Piece White Pawn, Piece White Pawn, Piece White Pawn, Piece White Pawn,
                                 Piece White Rook, Piece White Knight, Piece White Bishop, Piece White King, Piece White Queen, Piece White Bishop, Piece White Knight, Piece White Rook]
                boardCoords   = ["1a", "1b", "1c", "1d", "1e", "1f", "1g", "1h",
                                 "2a", "2b", "2c", "2d", "2e", "2f", "2g", "2h",
                                 "3a", "3b", "3c", "3d", "3e", "3f", "3g", "3h",
                                 "4a", "4b", "4c", "4d", "4e", "4f", "4g", "4h",
                                 "5a", "5b", "5c", "5d", "5e", "5f", "5g", "5h",
                                 "6a", "6b", "6c", "6d", "6e", "6f", "6g", "6h",
                                 "7a", "7b", "7c", "7d", "7e", "7f", "7g", "7h",
                                 "8a", "8b", "8c", "8d", "8e", "8f", "8g", "8h"]


drawBoard :: Board -> String
drawBoard b = "\n\n" ++ (concat $ digitLabel $ map (\x -> intercalate " | " x ++ "\n    -------------------------------\n") strings) ++ "\n" ++ letterLabel
    where
        strings = map (map show) $ sortFields b
        letterLabel = "     " ++ (intercalate "   " $ map (\x -> [x]) $ ['a'..'h'])
        digitLabel s = [(show (n+1)) ++ "    " ++ x | n <- [0..7], x <- [s!!n]]


sortFields :: Board -> [[Piece]]
sortFields (Board fs) =
  map (map piece) $
  map (sortBy (\x y -> compare (last $ coords x) (last $ coords y))) $
  chunksOf 8 $
  sortBy (\x y -> compare (head $ coords x) (head $ coords y)) fs

-- | [@whatPiece@] Takes a list of a board fields, and coordinates,
-- then iterates the fields' list in search for the exact coordinates.
-- Returns the Piece data of the coordinate.
whatPiece :: [Field] -> [Char] -> Piece
whatPiece [] _ = Piece NoColor NoType
whatPiece (f:fs) c
  | coords f == c = piece f
  | otherwise = whatPiece fs c


updateFields :: Board -> Field -> Field -> Board
updateFields b@(Board fs) ff@(Field fc _) sf@(Field sc _)
    | any ((\x -> coords x == fc)) fs = Board newFields
    | otherwise = b
    where
        newFields = sf : ff : [ x | x <- fs, coords x /= fc, coords x /= sc ]


otherPlayer :: PColor -> PColor
otherPlayer current = if current == White then Black else White

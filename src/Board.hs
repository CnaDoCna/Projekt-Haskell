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

data PColor = White | Black | NoColor
   deriving(Show, Eq)

instance Arbitrary PColor where
    arbitrary = elements [White, Black, NoColor]

data PType = King | Queen | Rook | Bishop | Knight | Pawn | NoType
   deriving(Show, Eq)
-- | Piece NoColor NoType is a Void Piece. I needed it to have color and type so it can match in functions' pattermatching!
instance Arbitrary PType where
     arbitrary = elements [King, Queen, Rook, Bishop, Knight, Pawn, NoType]

data Piece = Piece { colorof :: PColor, typeof :: PType}
  deriving(Eq)

instance Arbitrary Piece where
    arbitrary = do
      c <- arbitrary
      t <- arbitrary
      return $ Piece c t

data Field = Field { coords :: [Char], piece :: Piece }
   deriving(Show)

instance Arbitrary Field where
     arbitrary = do
         x <- elements ['1'..'8']
         y <- elements ['a'..'h']
         p <- arbitrary
         return $ Field [x,y] p

data Board = Board { fields :: [Field] }
   deriving(Show)

instance Arbitrary Board where
     arbitrary = do
         ps <- vectorOf 64 arbitrary
         return $ Board (map (\(p, [a,b]) -> Field [a,b] p) $ zip ps [[x,y] | x <- ['1'..'8'], y <- ['a'..'h']])


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
                boardCoords   = [[x,y] | x <- ['1'..'8'], y <- ['a'..'h']]


drawBoard :: Board -> String
drawBoard b = "\n" ++ (concat $ digitLabel $ map (\x -> intercalate " | " x ++ "\n    -------------------------------\n") strings) ++ "\n" ++ letterLabel
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
    | any ((\x -> coords x == fc)) fs && fc /= sc = Board newFields
    | otherwise = b
    where
        newFields = sf : ff : [ x | x <- fs, coords x /= fc, coords x /= sc ]


otherPlayer :: PColor -> PColor
otherPlayer current = if current == White then Black else White

module Board
    ( Piece (..)
    , Field (..)
    , Board (..)
    , PType (..)
    , PColor (..)
    , drawBoard
    , initialBoard
    , updateFields
    , whatPiece
    , nextPlayer
    ) where

import Data.List       (sortBy, intercalate)
import Data.List.Split (chunksOf)
import Test.QuickCheck
import Data.Functor
import Debug.Trace (traceShowId)

-- nie dodaję Empty bo mi nie pasuje w pattermatchingu
data PColor = White | Black | NoColor
   deriving(Show, Read, Eq)
data PType = King | Queen | Rook | Bishop | Knight | Pawn | NoType
   deriving(Show, Read, Eq)
data Piece = Piece { colorof :: PColor, typeof :: PType}
  deriving(Eq)

data Field =
    Field { coords :: [Char], piece :: Piece }
  --  deriving(Show, Read, Eq)

data Board =
    Board { fields :: [Field] }
--    deriving(Show, Read, Eq)


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
  map (sortBy (\x y -> compare (coords x !! 1) (coords y  !! 1))) $
  chunksOf 8 $
  sortBy (\x y -> compare (coords x !! 0) (coords y  !! 0)) fs


--zwraca pionek który stoi na podancych koordynatach na podanej tablicy
whatPiece :: [Field] -> [Char] -> Piece
whatPiece [] _ = Piece NoColor NoType
whatPiece (f:fs) p
  | coords f == p = piece f
  | otherwise = whatPiece fs p


updateFields :: Board -> Field -> Field -> Board
updateFields b@(Board fs) fn@(Field cn _) fo@(Field co _)
    | any ((\x -> coords x == cn)) fs = Board newFields
    | otherwise = b
    where
        newFields = fo : fn : [ x | x <- fs, coords x /= cn, coords x /= co ]


nextPlayer :: PColor -> PColor
nextPlayer current = if current == White then Black else White

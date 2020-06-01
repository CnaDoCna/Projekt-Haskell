data Value = O | X | Empty
    deriving(Read, Eq)


instance Show Value where
    show val = case val of
        O     -> "O"
        X     -> "X"
        Empty -> " "


data Field a =
    Field { coords :: (Int, Int), value :: a }
    deriving(Show, Read, Eq)


instance Functor Field where
     fmap f (Field cs v) = Field cs (f v)


instance Applicative Field where
     pure a = Field (0,0) a
     (Field csl f) <*> (Field cs2 v) = Field cs2 (f v) --aplikuje funkcje w typie Field do innego Field
--[(*3), (+10)] <*> [1,2,4] - wykonuje wszystkie funkcje dla listy - lista tez jest funktorem

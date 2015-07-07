module Grid where

import Data.List
import Data.Foldable (Foldable(..)) 
import Control.Applicative

transposeString :: String -> String
transposeString str = concat . mergeFirsts . splitOn (=='\n') $ str

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

safeHead def [] = def
safeHead _ (x:xs) = x
safeTail [] = []
safeTail (x:xs) = xs

mergeFirsts :: [String] -> [String]
mergeFirsts []      = []
mergeFirsts ([]:xs) = map (' ':) $ mergeFirsts xs
mergeFirsts xss = (map (safeHead ' ') xss ++ "\n") : (mergeFirsts . map safeTail $ xss)

zipWithDefault :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault da db op la lb = take len $ zipWith op la' lb'
    where
        len = max (length la) (length lb)
        la' = la ++ (repeat da)
        lb' = lb ++ (repeat db)

padTo :: Int -> String -> String
padTo n str = if cur < n
    then str ++ (take (n - cur) $ repeat ' ')
        else str
    where cur = length str

splitHalf :: [a] -> ([a],[a])
splitHalf ls = splitAt ((length ls + 1) `div` 2) ls

data Grid a = Square a
            | HLine [a]
            | VLine [a]
            | Region [[a]]
            | HSplit (Grid a) (Grid a)
            | VSplit (Grid a) (Grid a)

gridWidth :: Show a => Grid a -> Int
gridWidth (Square x)           = length (show x)
gridWidth (HLine [])           = 0
gridWidth (HLine (x:xs))       = length (show x) + gridWidth (HLine xs)
gridWidth (VLine [])           = 0
gridWidth (VLine ys)           = maximum $ map (length . show) ys
gridWidth (Region [])          = 0
gridWidth (Region xss)         = maximum (map (gridWidth . HLine) xss)
gridWidth (HSplit tGrid bGrid) = maximum [gridWidth tGrid, gridWidth bGrid]
gridWidth (VSplit lGrid rGrid) = gridWidth lGrid + gridWidth rGrid

gridHeight :: Grid a -> Int
gridHeight (Square x)           = 1
gridHeight (HLine [])           = 0
gridHeight (HLine (x:xs))       = 1
gridHeight (VLine [])           = 1
gridHeight (VLine ys)           = length ys
gridHeight (Region [])          = 0
gridHeight (Region xss)         = length xss
gridHeight (HSplit tGrid bGrid) = gridHeight tGrid + gridHeight bGrid
gridHeight (VSplit lGrid rGrid) = maximum [gridHeight lGrid, gridHeight rGrid]

instance Show a => Show (Grid a) where
    show (Square x)           = show x ++ "\n"
    show (HLine [])           = "\n"
    show (HLine (x:xs))       = show x ++ show (HLine xs)
    show (VLine [])           = ""
    show (VLine ys)           = (intercalate "\n" . map show $ ys) ++ "\n"
    show (Region [])          = ""
    show (Region (x:xs))      = show (HLine x) ++ show (Region xs)
    show (HSplit tGrid bGrid) = show tGrid ++ show bGrid
    show (VSplit lGrid rGrid) = (intercalate "\n" $ zipWithDefault leftBlanks rightBlanks (++) lvsection rvsection) ++ "\n"
        where
            leftBlanks  = take (gridWidth lGrid) . repeat $ ' '
            rightBlanks = take (gridWidth rGrid) . repeat $ ' '
            lvsection   = map (padTo (gridWidth lGrid)) . splitOn (=='\n') . show $lGrid
            rvsection   = map (padTo (gridWidth lGrid)) . splitOn (=='\n') . show $ rGrid

instance Functor Grid where
    fmap f (Square x)           = Square (f x)
    fmap _ (HLine [])           = HLine []
    fmap f (HLine xs)           = HLine (fmap f xs)
    fmap _ (VLine [])           = VLine []
    fmap f (VLine ys)           = VLine (fmap f ys)
    fmap _ (Region [])          = Region []
    fmap f (Region xys)         = Region (map (fmap f) $ xys)
    fmap f (HSplit tGrid bGrid) = HSplit (fmap f tGrid) (fmap f bGrid)
    fmap f (VSplit lGrid rGrid) = VSplit (fmap f lGrid) (fmap f rGrid)

instance Applicative Grid where
    pure x                              = Square x
    -- Squares are simplest of all
    (<*>) (Square f) grid               = fmap f grid
    -- Horizontal lines
    (<*>) (HLine (f:_)) (Square x)      = Square (f x)
    (<*>) (HLine fs) (HLine xs)         = HLine (zipWith ($) fs xs)
    (<*>) (HLine fs) (VLine ys)         = VLine (zipWith ($) fs ys)
    (<*>) (HLine fs) (Region xys)       = Region (map (zipWith ($) fs) xys)
    (<*>) (HLine fs) (HSplit tg bg)     = HSplit (HLine fs <*> tg) (HLine fs <*> bg)
    (<*>) (HLine fs) (VSplit lg rg)     = VSplit (HLine fs <*> lg) (HLine fs <*> rg)
    -- Vertical lines
    (<*>) (VLine (f:_)) (Square x)      = Square (f x)
    (<*>) (VLine fs) (HLine xs)         = HLine (zipWith ($) fs xs)
    (<*>) (VLine fs) (VLine ys)         = VLine (zipWith ($) fs ys)
    (<*>) (VLine fs) (Region xys)       = Region (zipWith map fs xys)
    (<*>) (VLine fs) (HSplit tg bg)     = HSplit (VLine fs <*> tg) (VLine fs <*> bg)
    (<*>) (VLine fs) (VSplit lg rg)     = VSplit (VLine fs <*> lg) (VLine fs <*> rg)
    -- Regions
    (<*>) (Region ((f:_):_)) (Square x) = Square (f x)
    (<*>) (Region (fs:_)) (HLine xs)    = HLine (zipWith ($) fs xs)
    (<*>) (Region fss) (VLine ys)       = VLine (zipWith ($) (map head fss) ys)
    (<*>) (Region fss) (Region xys)     = Region (zipWith (zipWith ($)) fss xys)
    (<*>) (Region fss) (HSplit tg bg)   = HSplit (Region fss <*> tg) (Region fss <*> bg)
    (<*>) (Region fss) (VSplit lg rg)   = VSplit (Region fss <*> lg) (Region fss <*> rg)
    -- Splits (retains split structure and distributes grids, breaking paradigm)
    (<*>) (HSplit tf bf) (Square x)     = HSplit (tf <*> Square x) (bf <*> Square x)
    (<*>) (HSplit tf bf) (HLine xs)     = HSplit (tf <*> HLine xs) (bf <*> HLine xs)
    (<*>) (HSplit tf bf) (VLine xs)     = HSplit (tf <*> VLine xs) (bf <*> VLine xs)
    (<*>) (HSplit tf bf) (Region xss)   = HSplit (tf <*> Region xss) (bf <*> Region xss)
    (<*>) (HSplit tf bf) (HSplit tg bg) = HSplit (tf <*> tg) (bf <*> bg)
    (<*>) (HSplit tf bf) (VSplit lg rg) = HSplit (tf <*> lg) (bf <*> rg)
    (<*>) (VSplit lf rf) (Square x)     = VSplit (lf <*> Square x) (rf <*> Square x)
    (<*>) (VSplit lf rf) (HLine xs)     = VSplit (lf <*> HLine xs) (rf <*> HLine xs)
    (<*>) (VSplit lf rf) (VLine xs)     = VSplit (lf <*> VLine xs) (rf <*> VLine xs)
    (<*>) (VSplit lf rf) (Region xss)   = VSplit (lf <*> Region xss) (rf <*> Region xss)
    (<*>) (VSplit lf rf) (HSplit tg bg) = VSplit (lf <*> tg) (rf <*> bg)
    (<*>) (VSplit lf rf) (VSplit lg rg) = VSplit (lf <*> lg) (rf <*> rg)

instance Monad Grid where
    return x                    = Square x
    (>>=) (Square x) f          = f x
    (>>=) (HLine []) _          = HLine [] 
    (>>=) (HLine [x]) f         = f x
    (>>=) (HLine xs) f          = VSplit (HLine (fst . splitHalf $ xs) >>= f) (HLine (snd . splitHalf $ xs) >>= f)
    (>>=) (VLine []) _          = VLine [] 
    (>>=) (VLine [x]) f         = f x
    (>>=) (VLine xs) f          = HSplit (VLine (fst . splitHalf $ xs) >>= f) (VLine (snd . splitHalf $ xs) >>= f)
    (>>=) (Region []) _         = Region []
    (>>=) (Region [xs]) f       = HLine xs >>= f
    (>>=) (Region (xs:xys)) f   = HSplit (HLine xs >>= f) (Region xys >>= f)
    (>>=) (HSplit tg bg) f      = HSplit (tg >>= f) (bg >>= f)
    (>>=) (VSplit lg rg) f      = VSplit (lg >>= f) (rg >>= f)

-- Grid should not have a Monoid instance, as there is no identity grid (empty grid) for concatenation of grids
-- Grid could, however, have a Semigroup instance, as grids could be appended to one another
-- This is avoided though, since the implmentation would prefer either of HSplit or VSplit
-- The implication is that there are two equally valid interpretations for the binary operator (AddWithH or AddWithV)
-- Another potential semigroup interpretation is that of combining elements of compatibly structured grids
-- This unfortunately depends on properties of the Grid elements, which should never be a requirement

instance Foldable Grid where
    foldr f i (Square x)     = f x i
    foldr _ i (HLine [])     = i
    foldr f i (HLine (x:xs)) = f x (Data.List.foldr f i xs)
    foldr _ i (VLine [])     = i
    foldr f i (VLine (y:ys)) = f y (Data.List.foldr f i ys)
    foldr _ i (Region [])    = i
    foldr f i (Region xys)   = Data.List.foldr (\xs i -> Data.List.foldr f i xs) i xys
    foldr f i (HSplit tg bg) = Data.Foldable.foldr f (Data.Foldable.foldr f i tg) bg
    foldr f i (VSplit lg rg) = Data.Foldable.foldr f (Data.Foldable.foldr f i lg) rg

testGrid :: Grid Int
testGrid = Region [[1,2,3],[4,5,6],[7,8,9]]

makeRegion :: Int -> Int -> Int -> Grid Int
makeRegion w h val = Region (take h . repeat $ (take w . repeat $ val))
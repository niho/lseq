module Data.LSeq
  ( LSeq
  , alloc
  , empty
  , size
  , insert
  , remove
  , fromList
  , Data.LSeq.toList
  ) where

import Prelude hiding (sequence)
import Data.Data (Data, Typeable)
import Data.Foldable (foldl')
import Data.Digits (digits, unDigits)
import System.Random
import Debug.Trace

type Id = [Integer]

data Strategy = BPlus | BMinus deriving (Show, Enum, Bounded, Eq)
data Leaf a = Value (Id, a) | Start | End deriving (Show)
data LSeq a = LSeq { sequence :: [Leaf a]
                   , strategy :: [Strategy]
                   , boundary :: Integer
                   , counter :: Integer
                   , base :: Int
                   , rgen :: StdGen } deriving (Show)

--instance (Show a) => Show (LSeq a) where
--  show = show . sequence

instance Random Strategy where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

alloc :: Id -> Id -> LSeq a -> ((Id, StdGen), [Strategy])
alloc p q lseq =
  case (strategy' !! depth) of
    BPlus -> (bPlus p depth step rgen', strategy')
    BMinus -> (bMinus q depth step rgen', strategy')
  where
    (depth, interval) = getLevel p q 0 [0]
    step = min (boundary lseq) (sum interval)
    (s, rgen') = random (rgen lseq)
    strategy' = [ if i < length (strategy lseq) then (strategy lseq) !! i else s | i <- [0..depth] ]

bPlus :: Id -> Int -> Integer -> StdGen -> (Id, StdGen)
bPlus p depth step g =
  (a ++ (map (\x -> x + addVal + 1) b), g')
    where
      (addVal, g') = randomR (0, step) g
      (a, b) = splitAt depth (prefix p depth)

bMinus :: Id -> Int -> Integer -> StdGen -> (Id, StdGen)
bMinus q depth step g =
  (a ++ (map (\x -> x - subVal + 1) b), g')
    where
      (subVal, g') = randomR (0, step) g
      (a, b) = splitAt depth (prefix q depth)

leafId :: Leaf a -> Id
leafId leaf =
  case leaf of
    Start -> [0]
    End -> [100]
    Value (id, _) -> id

--getBitBase :: Int -> Int
--getBitBase depth = 3 + level

getLevel :: Id -> Id -> Int -> Id -> (Int, Id)
getLevel p q depth interval =
  -- trace ( "- p = " ++ show p ++
  --         ", q = " ++ show q ++
  --         ", depth = " ++ show depth ++
  --         ", interval = " ++ show interval ++
  --         ", sum = " ++ show (sum interval) ++
  --         ", q^prefix = " ++ show (prefix q depth) ++
  --         ", p^prefix = " ++ show (prefix p depth) ++ "\n") $
  if (sum interval) < 1 && depth < 64
    then getLevel p q (depth + 1) (idiff (prefix q depth) (prefix p depth))
    else (depth, interval)

idiff :: Id -> Id -> Id
idiff p q = [(sum p) - (sum q) - 1]

prefix :: Id -> Int -> Id
prefix id depth =
  [ if i < length id then id !! i else 0 | i <- [0..depth] ]

get :: Int -> LSeq a -> Leaf a
get index (LSeq { sequence = seq }) = seq !! index

empty :: StdGen -> LSeq a
empty g = LSeq { sequence = [Start, End]
             , strategy = []
             , boundary = 10
             , counter = 0
             , base = 3
             , rgen = g
             }

size :: LSeq a -> Integer
size = counter

insert :: a -> Int -> LSeq a -> LSeq a
insert element index lseq =
  lseq { sequence = applyInsert' element id (sequence lseq)
       , counter = (counter lseq) + 1
       , strategy = s
       , rgen = g
       }
    where
      p = leafId (get index lseq)
      q = leafId (get (index+1) lseq)
      ((id, g), s) = alloc p q lseq

remove :: Int -> LSeq a -> LSeq a
remove index lseq =
  lseq { sequence = drop 1 (sequence lseq) }

applyInsert :: a -> Id -> LSeq a -> LSeq a
applyInsert element id lseq =
  lseq { sequence = applyInsert' element id (sequence lseq)
       , counter = (counter lseq) + 1
       }

applyInsert' :: a -> Id -> [Leaf a] -> [Leaf a]
applyInsert' element id xs =
  case binaryIndexOf id xs of
    Nothing ->
      Value (id, element) : xs
    Just index ->
      let (ys,zs) = splitAt index xs in ys ++ [Value (id, element)] ++ zs


-- applyRemove

binaryIndexOf :: Id -> [Leaf a] -> Maybe Int
binaryIndexOf id list =
  binsearch (map leafId list) id 0 ((length list) - 1)

binsearch :: (Ord a) => [a] -> a -> Int -> Int -> Int
binsearch xs value low high
  | high < low       = -1
  | xs!!mid > value  = binsearch xs value low (mid-1)
  | xs!!mid < value  = binsearch xs value (mid+1) high
  | otherwise        = mid
  where
    mid = low + ((high - low) `div` 2)

fromList :: [a] -> StdGen -> LSeq a
fromList list g = foldl' (\lseq x -> insert x 0 lseq) (empty g) (reverse list)

toList :: LSeq a -> [a]
toList lseq = foldl' (\list leaf ->
  case leaf of
    Start -> list
    End -> list
    Value (_, value) -> list ++ [value]
  ) [] (sequence lseq)

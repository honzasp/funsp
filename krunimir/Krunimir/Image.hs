module Krunimir.Image
( Image(..)
, Segment(..)
, limit
) where

data Image
  = EmptyImg
  | SplitImg Image Image
  | SegmentImg Segment Image
  deriving Show

data Segment = Segment Point Point Color Int 
  deriving Show

type Point = (Int,Int)
type Color = (Int,Int,Int)

limit :: Integer -> Image -> Image
limit n img
  | n <= 0 = EmptyImg
  | otherwise = case img of
    EmptyImg -> EmptyImg
    SplitImg l r -> SplitImg (limit n l) (limit n r)
    SegmentImg seg x -> SegmentImg seg $ limit (n-1) x

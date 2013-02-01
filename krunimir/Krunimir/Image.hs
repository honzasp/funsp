module Krunimir.Image
( Image(..)
, Segment(..)
, limit
, imageToSegss
, imageNodes
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

imageToSegss :: Image -> [[Segment]]
imageToSegss EmptyImg = []
imageToSegss (SegmentImg seg img) = [seg]:imageToSegss img
imageToSegss (SplitImg limg rimg) = zipSegs (imageToSegss limg) (imageToSegss rimg)
  where
  zipSegs :: [[Segment]] -> [[Segment]] -> [[Segment]]
  zipSegs [] [] = []
  zipSegs ls [] = ls
  zipSegs [] rs = rs
  zipSegs (l:ls) (r:rs) = (l ++ r):zipSegs ls rs

imageNodes :: Image -> Integer
imageNodes EmptyImg = 1
imageNodes (SegmentImg seg img) = 1 + imageNodes img
imageNodes (SplitImg limg rimg) = 1 + imageNodes limg + imageNodes rimg

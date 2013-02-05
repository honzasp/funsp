\begin{code}
module Krunimir.Image
( Image(..)
, Segment(..)
, prune
, imageToSegss
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

prune :: Integer -> Image -> Image
prune n img
  | n <= 0 = EmptyImg
  | otherwise = case img of
    EmptyImg -> EmptyImg
    SplitImg l r -> SplitImg (prune n l) (prune n r)
    SegmentImg seg x -> SegmentImg seg $ prune (n-1) x

imageToSegss :: Image -> [[Segment]]
imageToSegss EmptyImg = []
imageToSegss (SegmentImg seg img) = [seg]:imageToSegss img
imageToSegss (SplitImg limg rimg) = zipSegs (imageToSegss limg) (imageToSegss rimg)
  where
  zipSegs :: [[Segment]] -> [[Segment]] -> [[Segment]]
  zipSegs [] [] = []
  zipSegs lss [] = lss
  zipSegs [] rss = rss
  zipSegs (ls:lss) (rs:rss) = (ls ++ rs):zipSegs lss rss
\end{code}

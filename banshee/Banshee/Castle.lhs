\section{\texorpdfstring{@t{Banshee.Castle}}{Banshee.Castle}}
@Idx{Banshee.Castle}

\begin{code}
module Banshee.Castle where
import Data.Array
import Control.Monad
\end{code}

@Idx{Banshee.Castle.Castle}
@Idx{Banshee.Castle.Scout}
@Idx{Banshee.Castle.Loc}
@Idx{Banshee.Castle.Field}
@Idx{Banshee.Castle.Free}
@Idx{Banshee.Castle.Wall}
\begin{code}
data Castle = Castle
  { castleFields :: Array Loc Field
  , castleTV :: Loc
  , castleStart :: Loc
  , castleScouts :: [Scout]
  } 

type Scout = [Loc]
type Loc = (Int,Int)
data Field = Free | Wall deriving (Eq,Show)
\end{code}

@Idx{Banshee.Castle.Slice}
@Idx{Banshee.Castle.SliceField}
@Idx{Banshee.Castle.FreeSF}
@Idx{Banshee.Castle.WallSF}
@Idx{Banshee.Castle.ScoutSF}
\begin{code}
newtype Slice = Slice (Array Loc SliceField)
data SliceField = FreeSF | WallSF | ScoutSF Loc deriving (Eq,Show) 
\end{code}

@Idx{Banshee.Castle.sliceCastle}
\begin{code}
sliceCastle :: Castle -> [Slice]
sliceCastle castle = map slice [0..period-1] where
  fields = castleFields castle
  period = foldl lcm 1 . map length . castleScouts $ castle
  loopedScouts = map cycle $ castleScouts castle
  sfFields = fmap fieldToSF fields

  slice s = Slice $ sfFields // 
    [((x,y),ScoutSF next) 
    | scout <- loopedScouts
    , let (x,y):next:_ = drop s scout]

  fieldToSF Free = FreeSF
  fieldToSF Wall = WallSF
\end{code}

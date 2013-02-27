module Banshee.Castle where
import Data.Array

data Castle = Castle
  { castleFields :: Array Loc Field
  , castleTV :: Loc
  , castleStart :: Loc
  , castleScouts :: [Scout]
  }

data Scout = Scout [Loc] deriving Show

type Loc = (Int,Int)
type Pos = (Int,Int,Int)

data Field = Free | Wall deriving (Eq,Ord,Enum,Ix,Show)

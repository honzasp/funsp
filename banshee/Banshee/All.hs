module Banshee.All where
import Banshee.Castle
import Banshee.CastleParser
import Banshee.Navigate
import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Applicative
import Control.Monad.ST

rnd = do
  Right castle <- parseCastle "vstup" <$> readFile "test/data_testovaci/vstupy/4se_zvedy_skrz_zdi/vstup8.in"
  return $ navigate castle (slices castle) True

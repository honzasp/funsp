\section{\texorpdfstring{@t{Banshee.Navigate}}{Banshee.Navigate}}
@Idx{Banshee.Navigate}

\begin{code}
module Banshee.Navigate(navigate) where
import Data.Array
import Data.Array.ST
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.ST

import Banshee.Castle
\end{code}

@Idx{Banshee.Navigate.Path}
@Idx{Banshee.Navigate.pathLength}
\begin{code}
data Path = Path Int [Loc] deriving Show

pathLength (Path len _) = len
\end{code}

\subsection{Určení možných pohybů}

Funkce @t{moves} slouží k určení všech možných pozic, na které se bílá paní může
dostat z dané pozice. Tato funkce má typ @t{moves :: Bool -> Slice -> Slice ->
(Loc,Path) -> [(Loc,Path)]}:

\begin{itemize}
\item První argument (typu @t{Bool}) určuje, jestli můžeme procházet zdmi.
\item Druhý argument (@t{Slice}) je řez hradu v čase, ze kterého vycházíme.
\item Třetí argument (@t{Slice}) je řez hradu v dalším kroku (v čase, ve kterém
dorazíme na další políčko).
\item Poslední argument (@t{(Loc,Path)}) je dvojice -- pozice, ze které
vycházíme, a cesta, kterou jsme se na tuto pozici dostali.
\item Výsledkem (@t{[(Loc,Path)]}) je seznam pozic, kam se můžeme dostat, spolu
s cestami.
\end{itemize}

@Idx{Banshee.Navigate.moves}
\begin{code}
moves :: Bool -> Slice -> Slice -> (Loc,Path) -> [(Loc,Path)]
moves thruWalls (Slice foreslice) (Slice afterslice) ((x,y),Path len ps) = 
  [((tox,toy),Path (len+1) $ (x,y):ps)
    | (tox,toy) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    , tox >= 1 && tox <= width
    , toy >= 1 && toy <= height
    , case afterslice ! (tox,toy) of
        FreeSF -> True
        WallSF -> thruWalls
        ScoutSF _ -> False
    , case foreslice ! (tox,toy) of
        ScoutSF scouts | (x,y) `elem` scouts -> False
        _ -> True
    ]
  where
    ((1,1),(width,height)) = bounds foreslice
\end{code}

Funkci jsme implementovali pomocí generátoru seznamu. Nejprve si vygenerujeme
dvojice @t{(tox,toy)} všech čtyř pozic sousedních výchozí pozici @t{(x,y)}.
Následně zkontrolujeme, že se tyto pozice nachází v hradu a podíváme se, co nás
na této pozici v hradu čeká.

Je-li to volné políčko, je vše v pořádku. Pokud narazíme na zeď, tak pokračujeme
pouze pokud má argument @t{thruWalls} hodnotu @t{True}, pokud by se ale bílá
paní dostala na stejné políčko se zvědem, musíme tuto pozici přeskočit.

Nakonec zkontrolujeme, jestli se v čase, ve kterém jsme vyšli, na políčku, na
které jdeme, nevyskytují zvědi. Pokud ano, a některý z těchto zvědů se v dalším
tahu přesune na pole, ze kterého jsme vyšli, znamená to, že si bílá paní se
zvědem prohodí místo, což znamená, že ji zvěd objeví.

\subsection{\texorpdfstring{Monáda @t{ST}}{Monáda ST}}

V našem algoritmu budeme muset dále pracovat s polem, ve kterém si budeme
ukládat nejlepší cesty, které jsme nalezli do každého políčka v hradě. Mohli
bychom použít klasické pole @t{Array}, ale protože do tohoto pole vždy zapíšeme
pouze jeden prvek, byl by program velmi neefektivní, jelikož při každém
\uv{zápisu} musíme celé pole zkopírovat.

Proto využijeme monádu @t{ST s}.\cite{launchbury1994lazy} Tento typ je podobný
typu @t{IO} v tom, že umožňuje akcím v této monádě využívat měnitelné datové
struktury, narozdíl od @t{IO} jsou však akce v monádě @t{ST s} omezeny pouze na
\uv{vnitřní} stav, nemohou tedy např. přistupovat k disku nebo vypisovat znaky
na obrazovku.  Nejdůležitější ale je, že výsledek akce v monádě @t{ST s} můžeme
získat z \emph{čistého} kódu.

Na akci typu @t{ST s a} je tedy možno pohlížet jako na \emph{deterministický
výpočet}, jenž využívá interní stav @t{s} a jehož výstup má typ @t{a}. Proměnná
@t{s} slouží k zajištění, že tento interní stav \uv{neunikne} z monády @t{ST}.
Funkce, která nám umožní provést výpočet v monádě @t{ST} a získat jeho výstup se
jmenuje @t{runST} a má typ @t{(forall s. ST s a) -> a}.\footnote{Kvantifikátor
@t{forall} je součástí typové magie zajišťující bezpečnost použití @t{ST} a je
součástí rozšíření jazyka @t{ExistentialQuantification}.}

\subsubsection{\texorpdfstring
{Měnitelné pole @t{STArray}}
{Měnitelné pole STArray}}

Měnitelné pole, které můžeme použít v monádě @t{ST s}, má typ @t{STArray s i e},
kde @t{s} je stejná typová proměnná, kterou předáme i do @t{ST s}, @t{i} je typ
indexu a @t{e} typ prvků.

S takovýmto polem můžeme provádět následující operace:

\begin{description}

\item[@t{getBounds :: STArray s i e -> ST s (i,i)}] \hfill \\
@t{getBounds ary} získá rozsah indexů pole @t{ary}.

\item[@t{newArray :: (i,i) -> e -> ST s (STArray s i e)}] \hfill \\
@t{newArray (a,a) x} vytvoří nové pole s rozsahem indexů od @t{a} do @t{b},
jehož prvky jsou inicializované na hodnotu @t{x}.

\item[@t{readArray :: STArray s i e -> i -> ST s e}] \hfill \\
@t{readArray ary i} přečte hodnotu prvku na indexu @t{i} v poli @t{ary}.

\item[@t{writeArray :: STArray s i e -> i -> e -> ST s ()}] \hfill \\
@t{writeArray ary i x} zapíše hodnotu @t{x} do indexu @t{i} v poli @t{ary}.

\end{description}

\subsection{Hledání cest v souvislých oblastech bez průchodu zdí}

@Idx{Banshee.Navigate.flood}
\begin{code}
flood :: Castle -> [Slice] -> STArray s (Int,Loc) (Maybe Path) ->
  [(Loc,Path)] -> ST s (Either Path [(Loc,Path)])
flood castle slices bests = step 0 (cycle slices)
  where
  step _ _ [] = return $ Right []
  step t (slice1:slice2:slices) locpaths = do
    let (starts,rest) = span ((<= t) . pathLength . snd) locpaths
        offset = t `mod` period

    (starts',nextss) <- fmap (unzip . catMaybes) $ forM starts $ \((x,y),path) -> do
      uncov <- isNothing <$> readArray bests (offset,(x,y))
      if uncov then do
          writeArray bests (offset,(x,y)) $ Just path
          return . Just . (,) ((x,y),path) $ moves False slice1 slice2 ((x,y),path)
        else return Nothing
    
    tv <- readArray bests (offset,castleTV castle)
    case tv of
      Nothing -> (fmap (starts'++)) <$> step (t+1) (slice2:slices) (concat nextss ++ rest)
      Just path -> return $ Left path

  period = length slices
\end{code}

\subsection{Hledání cest včetně procházení zdí}

@Idx{Banshee.Navigate.navigate}
\begin{code}
navigate :: Castle -> [Slice] -> Bool -> Maybe [Loc]
navigate castle slices thruWalls = runST $ do
  let ((1,1),(width,height)) = bounds $ castleFields castle
      start = [(castleStart castle,Path 0 [])]
  bests <- newArray ((0,(1,1)),(period-1,(width,height))) Nothing
  result <- if thruWalls
    then wallStep bests start
    else flood castle slices bests start
  case result of
    Left (Path len locs) -> return . Just . reverse $ castleTV castle : locs
    Right _ -> return Nothing

  where
  wallStep :: STArray s (Int,Loc) (Maybe Path) -> [(Loc,Path)] ->
    ST s (Either Path [(Loc,Path)])
  wallStep bests locpaths = do
    result <- flood castle slices bests locpaths
    case result of
      Right locpaths'@(_:_) -> do
        let nextss = (`map` locpaths') $ \((x,y),path@(Path len _)) ->
              let offset = len `mod` period
                  offset' = (len+1) `mod` period
              in  moves True (slices !! offset) (slices !! offset') ((x,y),path)
        wallStep bests (concat nextss)
      Left _ -> return result

  period = length slices
\end{code}

\section{\texorpdfstring{@t{Banshee.Navigate}}{Banshee.Navigate}}
@Idx{Banshee.Navigate}

\subsection{Popis algoritmu}

Náš algoritmus hledání cesty je založen na prohledávání do šířky s počátkem na
startovní pozici bílé paní. O cestě hradem můžeme uvažovat jako o sérii změny
\uv{časopozic}, kde časopozicí budeme nazývat dvojici \emph{pozice} v hradu a
\emph{času}, potřebného k cestě na tuto pozici modulo perioda hradu.  V hradu je
tedy celkem $w \times h \times p$ časopozic, kde $w$ je šířka hradu, $h$ je
výška a $p$ je perioda. 

V průběhu výpočtu si budeme udržovat pole, jež pro každou časopozici obsahuje
nejkratší cestu, jak této časopozice dosáhnout.  Pokud v každém kroku algoritmu
přiřadíme jedné časopozici nejkratší cestu, dostaneme časovou složitost
$O(whp)$, tedy přímo úměrnou počtu políček v hradu a periodě.

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

\subsection{Určení možných pohybů z políčka}

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
ukládat nejlepší cesty, které jsme nalezli do každé časopozice v hradě. Mohli
bychom použít klasické pole @t{Array}, ale protože do tohoto pole budeme
zapisovat po jednom prvku, byl by program velmi neefektivní, jelikož při každém
zápisu se celé pole musí zkopírovat.

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
součástí rozšíření @t{ExistentialQuantification} jazyka Haskell.}

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

Nyní si implementujeme funkci, která pro daný seznam výchozích pozic s
příslušnými cestami nalezne nejlepší cestu do každé časopozice hradu, kam se
můžeme dostat bez průchodu zdí. Tyto nejlepší cesty zapíše do předaného
měnitelného pole @t{STArray} a vrátí buď cestu k televizoru, pokud byla takováto
nalezena, nebo seznam dosažených pozic s příslušnými cestami.

Pro předaný seznam výchozích pozic a cest musí platit, že všechny cesty prochází
stejným počtem zdí a jsou seřazeny podle délky, což znamená, že lepší cesta vždy
v tomto seznamu předchází cestu horší.

Jednotlivé argumenty této funkce jsou následující:

\begin{itemize}
\item První argument (typ @t{Castle}) je hrad, ve kterém hledáme cestu.
\item Druhý argument (@t{[Slice]}) je seznam řezů, na které je tento hrad
nakrájen.
\item Třetí argument (@t{STArray s (Int,Loc) (Maybe Path)}) je měnitelné pole
@t{bests}, které každé časopozici přiřazuje nejlepší cestu (pokud existuje).
\item Poslední argument (@t{[(Loc,Path)]}) je seznam startovních pozic
seřazených podle délky (tyto ještě nejsou zapsány v poli @t{bests}).
\item Výsledek (@t{ST s (Either Path [(Loc,Path)])}) je výpočet v monádě @t{ST
s}, jehož výsledek je buď cesta k televizoru (hodnota @t{Left}) nebo seznam
pozic a cest, jež byly přidány do pole @t{bests}, seřazených podle délky.
\end{itemize}

@Idx{Banshee.Navigate.flood}
\begin{code}
flood :: Castle -> [Slice] -> STArray s (Int,Loc) (Maybe Path) ->
  [(Loc,Path)] -> ST s (Either Path [(Loc,Path)])
flood castle slices bests = step 0 (cycle slices)
  where
  step _ _ [] = return $ Right []
  step t (slice1:slice2:slices') locpaths = do
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
      Nothing -> (fmap (starts'++)) <$> step (t+1) (slice2:slices') (concat nextss ++ rest)
      Just path -> return $ Left path

  period = length slices
\end{code}

Téměř všechnu práci v této funkci vykoná pomocná funkce @t{step}. Jejím prvním
argumentem je čas @t{t}, ve kterém má začít, druhým nekonečný seznam řezů hradu
(prvním prvkem je řez v čase @t{t} -- @t{slice1}, druhým řez v čase @t{t+1} --
@t{slice2}) a posledním seznam dvojic pozic a cest do těchto pozic, ze kterých
hledáme cesty (@t{locpaths}).

Pokud jsme dostali tento seznam prázdný, funkce @t{step} jednoduše vrátí prázdný
seznam značící, že televizor nebyl nalezen a do pole @t{bests} nebylo přidáno
nic.

V opačném případě si nejprve seznam @t{locpaths} rozdělíme na dvě části --
seznam @t{starts} obsahuje všechny páry pozice-cesta ze kterých vyjdeme v tomto
kroku, v seznamu @t{rest} je zbytek.

Následně pro každou dvojici ze seznamu @t{starts} zkontrolujeme, jestli v poli
@t{bests} není náhodou již nalezena nějaká jiná nejlepší cesta. Pokud ne, do
pole zapíšeme tuto cestu a vrátíme jak tuto dvojici, tak seznam následujících
políček, \uv{zabalené} v konstruktoru @t{Just}. Pokud již byla nalezena lepší
cesta, vrátíme @t{Nothing}.

Tímto získáme seznam @t{[Maybe ((Loc,Path),[(Loc,Path)])]}, který následně
pomocí složené funkce @t{unzip . catMaybes} převedeme na
@t{([(Loc,Path)],[[(Loc,Path)]])}, tedy dvojici, jejíž první prvek je seznam
pozic a cest, které byly uznány za nejlepší (tento seznam označíme jako
@t{starts'}), a seznam seznamů políček a cest k nim, na které se z těchto
nejlepších cest můžeme dostat (v proměnné @t{nextss}).

Pak zkontrolujeme, jestli jsme neobjevili nejkratší cestu k televizoru. Pokud
ano, tak tuto cestu vrátíme jako @t{Left}, jinak rekurzivně zavoláme @t{step}
znovu a pomocí funkce @t{fmap} k jejímu výsledku přidáme ještě seznam
@t{starts'}.

\subsection{Hledání cest včetně procházení zdí}

Jako poslední implementujeme vlastní funkci @t{navigate}. Budeme jí předávat
hrad, seznam řezů tohoto hradu a hodnotu @t{Bool} značící, jestli povolíme
procházení zdí.

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
    Left (Path _ locs) -> return . Just . reverse $ castleTV castle : locs
    Right _ -> return Nothing

  where
  wallStep :: STArray s (Int,Loc) (Maybe Path) -> [(Loc,Path)] ->
    ST s (Either Path [(Loc,Path)])
  wallStep bests locpaths = do
    result <- flood castle slices bests locpaths
    case result of
      Left tvPath -> return $ Left tvPath
      Right [] -> return $ Right []
      Right locpaths' -> do
        let nexts = concat . (`map` locpaths') $ \((x,y),path@(Path len _)) ->
              let offset = len `mod` period
                  offset' = (len+1) `mod` period
              in  moves True (slices !! offset) (slices !! offset') ((x,y),path)
        wallStep bests nexts

  period = length slices
\end{code}

Veškerou práci ve funkci @t{navigate} provedeme v monádě @t{ST s}, pomocí
@t{runST} ji ale \uv{spustíme} a získáme výsledek. Poté, co zjistíme šířku a
výšku hradu a nadefinujeme seznam startovních pozic (@t{start}), vytvoříme pole
@t{bests}. Veškeré prvky tohoto pole budou nastaveny na hodnotu @t{Nothing}.

Pokud nemáme povoleno procházet skrz zdi (argument @t{thruWalls} má hodnotu
@t{False}), použijeme funkci @t{flood}, jelikož ta nikdy negeneruje cesty které
prochází zdmi. V opačném případě zavoláme pomocnou funkci @t{wallStep}, jež
najde i cesty skrz zdi a kterou si vzápětí popíšeme.

Ať už jsme použili @t{flood} nebo @t{wallStep}, získali jsme výsledek typu
@t{Either Path [(Loc,Path)]} v proměnné @t{result}. Pokud jsme dostali @t{Left},
byla nalezena cesta k televizi. K seznamu pozic z této cesty ještě přidáme
pozici televizoru a \uv{otočíme} ji tak, ať je pozice startu na prvním místě a
televize na místě posledním.  Hodnota @t{Right} značí, že cesta k televizoru
nebyla nalezena; v tom případě vrátíme @t{Nothing}.

\subsubsection{\texorpdfstring
  {Pomocná funkce @t{wallStep}}
  {Pomocná funkce wallStep}}

Funkci @t{wallStep} předáme jako argument pole @t{bests} a seznam startovních
pozic a cest. Nejprve zavoláme funkci @t{flood} a její výsledek přiřadíme
proměnné @t{result}. Pokud nalezla cestu k televizoru, jednoduše ji vrátíme.
Pokud takovou cestu nenalezla a zároveň nenašla cestu ani do jedné nové pozice v
hradě, znamená to, že už žádnou novou cestu nalézt nemůžeme a proto vrátíme
hodnotu @t{Right} značící neúspěch.

Pokud ale funkce @t{flood} nalezla cestu do nových pozic v hradě, seznam dvojic
těchto pozic a cest do nich uložíme do proměnné @t{locpaths'}. Následně každou z
těchto pozic projdeme a pomocí funkce @t{moves} (jíž jako první argument předáme
@t{True}) získáme seznam pozic, do kterých se z nich můžeme jedním pohybem i
skrz zeď dostat. Proměnná @t{nexts} tedy obsahuje seznam pozic a cest, které
můžou mít o jednu zeď v cestě více. S tímto seznamem poté znovu rekurzivně
zavoláme @t{wallStep}.

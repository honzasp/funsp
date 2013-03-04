\section{\texorpdfstring{@t{Banshee.Castle}}{Banshee.Castle}}
@Idx{Banshee.Castle}

V tomto modulu si nadefinujeme datové typy, které využijeme v ostatních
modulech.

\begin{code}
module Banshee.Castle where
import Data.Array
import Control.Monad
\end{code}

\subsection{Hrad}

Typ @t{Castle} reprezentuje hrad v podobě, v jaké jsme ho načetli ze souboru.
Všechna políčka jsou uložena v poli @t{castleFields} jako typ @t{Field}, který
může nabývat hodnot @t{Free} a @t{Wall}, tedy prázdné políčko či zeď.

Pole prvků typu @t{e} indexovaná typem @t{i} mají v Haskellu typ @t{Array i e}.
Index pole musí být instancí typové třídy @t{Ix}. Kromě celočíselných typů jako
@t{Int} či @t{Integer} tak můžeme použít i $n$-tice (např. @t{(Int,Int,Int)}) a
vytvořit tak vícerozměrné pole.

Indexem našeho pole @t{castleFields} je @t{Loc}, což je pouze synonym pro
@t{(Int,Int)} (dvojice celých čísel), pole je tedy dvojrozměrné.  Typ @t{Loc}
budeme používat pro reprezentaci pozice na mapě jako dvojice souřadnic
@t{(x,y)}. Levé horní políčko mapy má souřadnice @t{(1,1)}, pravé dolní
@t{(\textit{šířka mapy},\textit{výška mapy})}.

V @t{castleTV} je uložena pozice televizoru, v @t{castleStart} počáteční pozice
bílé paní. @t{castleScouts} obsahuje seznam zvědů (typ @t{Scout}), kteří jsou
reprezentování jako seznam umístění, která postupně navštěvují.

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
  } deriving Show

type Scout = [Loc]
type Loc = (Int,Int)
data Field = Free | Wall deriving (Eq,Show)
\end{code}

\subsubsection{Příklad}

Zde je bludiště z obrázku \ref{fig:banshee-castle-example} uložené jako typ
@t{Castle}.

\begin{haskell}
Castle 
  { castleFields = array ((1,1),(5,5))
      [((1,1),Free),((1,2),Wall),((1,3),Free),((1,4),Free),((1,5),Free)
      ,((2,1),Wall),((2,2),Wall),((2,3),Wall),((2,4),Free),((2,5),Free)
      ,((3,1),Free),((3,2),Free),((3,3),Free),((3,4),Free),((3,5),Wall)
      ,((4,1),Free),((4,2),Free),((4,3),Wall),((4,4),Wall),((4,5),Free)
      ,((5,1),Free),((5,2),Free),((5,3),Free),((5,4),Free),((5,5),Free)]
  , castleTV = (4,5)
  , castleStart = (1,1)
  , castleScouts = 
    [ [(5,4),(5,3),(5,2),(4,2),(5,2),(5,3)]
    , [(1,4),(2,4),(3,4),(2,4)]
    , [(4,2),(4,1),(5,1),(5,2)]]
  }
\end{haskell}

\subsection{Řezy hradu}

V průběhu hledání cesty budeme muset být schopni rychle určit, zda se v daném
čase nachází na daném políčku zvěd nebo ne. K tomu využijeme typ @t{Slice},
který reprezentuje \uv{řez časoprostorem hradu}, tedy umístění zdí, volných
políček a zvědů v daném čase (pohybují se samozřejmě pouze zvědové, zdi a volná
políčka zůstávají na svém místě, pouze mohou být \uv{překryta} zvědem).

Zvědové se na mapě pohybují po konstantní uzavřené dráze, což znamená, že pokud
je zvěd v čase $t$ na daném políčku, bude na tomto políčku i v čase $t+kp$, kde
$p$ je délka jeho trasy, pro všechna $k \in \mathbb{Z}$.

Z toho plyne, že po konečném počtu kroků se všichni zvědové z hradu dostanou
zpátky na své startovní pozice. Tomuto počtu kroků budeme říkat \emph{perioda} a
je roven nejmenšímu společnému násobku délek tras všech zvědů v
hradu.\footnote{Z původního zadání plyne, že délka tras všech zvědů musí být
sudá a že nepřesáhne 8, tudíž nejvyšší možná perioda je $2^3+3^1=24$. Náš
program akceptuje libovolně dlouhé trasy zvědů, periodu tedy budeme počítat pro
každý hrad zvlášť.}

Pro hrad s periodou $p$ tedy stačí vygenerovat prvních $p$ \uv{řezů}. Pro čas
$t$ získáme příslušný řez ze zbytku po dělení $t \div p$ (označíme-li počáteční
stav časem $t = 0$). 

Jednotlivé \uv{řezy} pro jednoduchou mapu jsou zobrazeny na obrázku
\ref{fig:banshee-castle-example}.

@Idx{Banshee.Castle.Slice}
@Idx{Banshee.Castle.SliceField}
@Idx{Banshee.Castle.FreeSF}
@Idx{Banshee.Castle.WallSF}
@Idx{Banshee.Castle.ScoutSF}
\begin{code}
newtype Slice = Slice (Array Loc SliceField) deriving Show
data SliceField = FreeSF | WallSF | ScoutSF [Loc] deriving (Eq,Show) 
\end{code}

Typ @t{Slice} je jen \uv{obalený} typ @t{Array Loc SliceField} (tzv.
\emph{newtype}). Od typového synonymu (uvozeného klíčovým slovem @t{type}) se
liší tím, že jej musíme explicitně \uv{rozbalovat} pomocí konstruktoru @t{Slice}
a že typ @t{Slice} není ekvivalentní typu @t{Array Loc SliceField}, můžeme pro
něj tedy definovat např. jiné typové třídy.\footnote{To je důvod, proč Haskell
takovéto typy zavádí.} Je tedy velmi podobný klasickému datovému typu (klíčové
slovo @t{data}) s jediným datovým konstruktorem s jediným prvkem.

@t{SliceField} je obdobou typu @t{Field}, rozdíl je v tom, že ve @t{SliceField}
je možno uložit i zvěda. Parametr typu @t{[Loc]} konstruktoru @t{ScoutSF} je
následující pozice zvědů nacházejících se na tomto políčku; využijeme ji při
zjišťování, zda si bílá paní a zvěd v jednom kroku nevyměnili pozice, což je
považováno za objevení bílé paní zvědem.

\subsection{Řezání hradu}

Pro samotné rozřezání časoprostoru v okolí hradu na tenké plátky využijeme
funkci @t{sliceCastle}.

@Idx{Banshee.Castle.sliceCastle}
\begin{code}
sliceCastle :: Castle -> [Slice]
sliceCastle castle = map slice [0..period-1] where
  fields = castleFields castle
  period = foldl lcm 1 . map length . castleScouts $ castle
  cycledScouts = map cycle $ castleScouts castle
  sfFields = fmap fieldToSF fields

  slice s = Slice $ accum acc sfFields
    [((x,y),next) 
    | scout <- cycledScouts
    , let (x,y):next:_ = drop s scout]

  acc :: SliceField -> Loc -> SliceField
  acc old scoutNext = case old of
    FreeSF -> ScoutSF [scoutNext]
    WallSF -> ScoutSF [scoutNext]
    ScoutSF scoutNexts' -> ScoutSF $ scoutNext:scoutNexts'

  fieldToSF Free = FreeSF
  fieldToSF Wall = WallSF
\end{code}

Pro přehlednost bude vhodné rozebrat si jednotlivé proměnné:

\begin{description}

\item[@t{fields}] je pole políček (@t{Array Loc Field}) získané z předaného
hradu.

\item[@t{period}] je délka periody, s jakou se pohybují všichni zvědové. Jedná
se o nejmenší společný násobek délek tras všech zvědů z hradu. (Funkce @t{lcm a
b} spočte nejvyšší společný násobek dvou čísel @t{a} a @t{b}.)

\item[@t{cycledScouts}] je seznam tras všech zvědů (@t{[Loc]}) \uv{zacyklený} do
nekonečně se opakující sekvence pomocí funkce @t{cycle}.

\item[@t{sfFields}] je pole @t{field} převedené z hodnot typu @t{Field} na
@t{FieldSF}. Obsahuje tedy stejnou informaci, jen jiného typu.

\item[@t{slice s}] je funkce, která pro čas @t{s} vrátí příslušný řez. Získáme
jej tak, že do \uv{čistého} pole @t{sfFields}, obsahujícího pouze @t{FreeSF} a
@t{WallSF}, přidáme na příslušné pozice i hodnoty @t{ScoutSF}, k čemuž použijeme
funkci @t{accum}.

Typ této funkce je @t{accum :: (e -> a -> e) -> Array i e -> [(i, a)] -> Array i
e}.\footnote{Pro jednoduchost jsme vynechali omezení třídy @t{Ix i => ...}}
První argument je akumulační funkce @ti{f}, druhým výchozí pole @ti{ary} a třetí
seznam asociací @ti{xs}. Výsledné pole obsahuje stejné prvky jako @ti{ary}, s
tím rozdílem, že pro každou asociaci index-hodnota @ti{(i,x)} ze seznamu @ti{xs}
zavolá funkci @ti{f}, která na základě předchozí hodnoty z pole a hodnoty @ti{x}
vrátí novou hodnotu, která se uloží ve výsledném poli na indexu @ti{i}. 

Důležité je, že stejný index se v seznamu @ti{xs} může objevit i vícekrát -- v
tom případě se funkci @ti{f} předá hodnota získaná z předchozí asociace. Takto
je tedy možno stejný prvek \uv{upravit} vícekrát.

Je důležité si uvědomit, že pole @ti{ary}, předané funkci @t{accum}, se nijak
nezmění, výsledek se předá v novém poli (jinak to ve funkcionálním jazyku ani
nejde).

V našem případě funkci @t{accum} předáme pomocnou akumulační funkci @t{acc},
pole @t{sfFields} a seznam vyjádřený pomocí generátoru seznamu, který obsahuje
dvojice @t{(\textit{p},\textit{q})}, kde \textit{p} je pozice zvěda v čase @t{s}
a \textit{q} je pozice stejného zvěda v čase @t{s+1}. Funkce @t{accum} tedy
pomocí funkce @t{acc} na pozici \textit{p} zapíše informaci o tom, že se zde
nachází zvěd, jehož další destinací je pozice @t{q}.

\item[@t{acc old scoutNext}] tedy udělá jednu ze dvou věcí: 
  \begin{inparaenum}[(a)]
    \item je-li políčko prázdné (@t{FreeSF}) nebo zeď (@t{WallSF}), nahradí jej
    hodnotou @t{ScoutSF} s jediným zvědem, jehož následující pozice je
    @t{scoutNext};
    \item pokud na políčku již byli zapsáni nějací zvědové, přidá k seznamu
    jejich následujících pozic i @t{scoutNext}.
  \end{inparaenum}

\item[@t{fieldToSF}] je pomocná funkce, kterou jsme využili v definici pole
@t{sfFields}.

\end{description}

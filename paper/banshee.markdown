---
layout: paper
title: "Kapitola 3 &mdash; Bílá paní: cesta hradem"
root: ..
---

# {{ page.title }}

> Bílá paní s bezhlavým rytířem společně sledovali bezhlavě prchající turisty.
> „A nenudíte se sama dole v podhradí,“ ptal se rytíř, „když všechny
> návštěvníky vyděsíte tak, že nestačí utíkat?“ „Ale ne, jedni mi tam nechali
> televizor a já na něm sleduji ty, jak se to jmenuje ... telenovely. Jenom teď
> musím ten televizor zase najít a v mém věku už není procházení zdí tak
> jednoduché jako zamlada.“ <a href="bib.html#b1" class="cite">[1]</a>

Soutěžní úloha z roku 2010 <a href="bib.html#b1" class="cite">[1]</a>
(přetisknuta v práci na straně 63) je variací na klasické téma hledání cesty v
bludišti.  Bílá paní se nachází na hradě se zdmi a chodbami a snaží se dojít k
televizoru, aby stihla začátek své oblíbené telenovely. Není ovšem sama -- v
hradě se pohybují zvědaví zvědové, kteří se snaží bílou paní odhalit, proto se
jim musí vyhnout. Bílá paní sice může procházet zdmi, ale snaží se procházení
omezit na minimum, dá přednost delší cestě s menším počtem zdí než kratší cestě
s větším počtem zdí.

## 3.1 Popis úlohy

Bludiště představuje hrad sestávající z volných políček a zdí, ve kterém se po
předem daných cyklických drahách pohybují zvědové. Je dána počáteční pozice bílé
paní a televizor, který představuje kýžený cíl. Bílá paní může procházet zdmi,
ale nesmí být objevena zvědem.

Cesta s počtem kroků <i>k<sub>1</sub></i>, která vede skrz <i>z<sub>1</sub></i>
zdí, je *lepší* než cesta s <i>k<sub>2</sub></i> kroky a <i>z<sub>2</sub></i>
zdmi právě tehdy, je-li <i>z<sub>1</sub> &lt; z<sub>2</sub></i> nebo
<i>z<sub>1</sub> = z<sub>2</sub> ∧ k<sub>1</sub> &lt; k<sub>2</sub></i>.
Platí-li <i>z<sub>1</sub> = z<sub>2</sub> ∧ k<sub>1</sub> = k<sub>2</sub></i>,
považujeme obě cesty za rovnocenné.  Úkolem je najít *nejlepší* cestu z
počáteční pozice k televizoru, aniž by byla objevena zvědem.



Zvěd bílou paní objeví tehdy, když (a) se oba ve stejném čase nachází na stejném
políčku, nebo (b) si v jednom kroku vymění pozice, tj. v čase <i>t</i> stojí
zvěd na políčku <i>A</i> a bílá paní na políčku <i>B</i> a v čase <i>t+1</i>
bílá paní na políčku <i>A</i> a *stejný* zvěd na políčku <i>B</i>.

Vstupem programu je soubor s bludištěm uloženým v klasickém formátu
programovacích soutěží, tj. na prvním řádku dvě čísla určující rozměry bludiště
a na dalších řádcích po znacích jednotlivá políčka -- `.` volné, `X` zeď, `&`
počáteční pozice bílé paní, `#` televizor. Zvěd je uložen jako znak `@`, za
kterým následuje jeho trasa jako sada pohybů `v`, `>`, `^` a `<` (na jih,
východ, sever a západ). 

Příklad mapy i s pohyby zvědů je na obrázku [3.1](#img-3.1).

<figure id="img-3.1">
  <div class="image"><img src="../img/banshee-example.svg" width="600"></div>
  <figcaption>
Obrázek 3.1: Příklad bludiště (převzatý z ukázkových souborů ze soutěže) s
rozfázovanými pohyby zvědů. Zvědové mají pohyby s periodou 4 a 6, což znamená,
že po 12 krocích jsou všichni znovu na stejných pozicích.
  </figcaption>
</figure>

## 3.2 Banshee

I když bychom mohli do angličtiny přeložit „bílou paní“ jako "white lady",
budeme o ní v programu hovořit jako o "banshee", což je v irské mytologii duch
ženy přicházející z podsvětí a předznamenávající blížící se smrt. Samozřejmě
nejde o ekvivalent klasické české hradní bílé paní, ale pro naše účely je název
`Banshee` vhodnější než `WhiteLady`.

## 3.3 Analýza

Stejně jako u Krunimíra i nyní bude vhodné program rozčlenit do modulů:

* `Banshee.Castle` definuje datové typy reprezentující hrad, které
  budeme používat v celém programu.

* `Banshee.CastleParser` poskytuje funkci `parseCastle`, která přečte
  popis hradu ze souboru ve výše uvedeném formátu.

* `Banshee.Navigate` obsahuje jádro programu -- funkci `navigate`,
  která najde cestu v předaném hradu.

* `Banshee.Main` exportuje `main`, která slouží jako uživatelské
  rozhraní programu.


## 3.4 `Banshee.Castle`

V tomto modulu si nadefinujeme datové typy, které využijeme v ostatních
modulech.

    module Banshee.Castle where
    import Data.Array

### 3.4.1 Hrad

Typ `Castle` reprezentuje hrad v podobě, v jaké jsme ho načetli ze souboru.
Všechna políčka jsou uložena v poli `castleFields` jako typ `Field`, který
může nabývat hodnot `Free` a `Wall`, tedy prázdné políčko či zeď.

    data Castle = Castle
      { castleFields :: Array Loc Field
      , castleTV :: Loc
      , castleStart :: Loc
      , castleScouts :: [Scout]
      } deriving Show

    type Scout = [Loc]
    type Loc = (Int,Int)
    data Field = Free | Wall deriving (Eq,Show)

Pole prvků typu `e` indexovaná typem `i` mají v Haskellu typ `Array i e`.  Index
pole musí být instancí typové třídy `Ix`. Kromě celočíselných typů jako `Int` či
`Integer` tak můžeme použít i <i>n</i>-tice (např. `(Int,Int,Int)`) a vytvořit
tak vícerozměrné pole.

Indexem našeho pole `castleFields` je `Loc`, což je pouze synonym pro
`(Int,Int)` (dvojice celých čísel), pole je tedy dvojrozměrné.  Typ `Loc`
budeme používat pro reprezentaci pozice na mapě jako dvojice souřadnic
`(x,y)`. Levé horní políčko mapy má souřadnice `(1,1)`, pravé dolní
<code>(<i>šířka mapy</i>,<i>výška mapy</i>)</code>.

V `castleTV` je uložena pozice televizoru, v `castleStart` počáteční pozice
bílé paní. `castleScouts` obsahuje seznam zvědů (typ `Scout`), kteří jsou
reprezentování jako seznam umístění, která postupně navštěvují.

#### Příklad

Zde je bludiště z obrázku [3.1](#img-3.1) uložené jako typ `Castle`.

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

### 3.4.2 Řezy hradu

V průběhu hledání cesty budeme muset být schopni rychle určit, zda se v daném
čase nachází na daném políčku zvěd nebo ne. K tomu využijeme typ `Slice`,
který reprezentuje „řez časoprostorem hradu“, tedy umístění zdí, volných
políček a zvědů v daném čase (pohybují se samozřejmě pouze zvědové, zdi a volná
políčka zůstávají na svém místě, pouze mohou být „překryta“ zvědem).

Zvědové se na mapě pohybují po konstantní uzavřené dráze, což znamená, že pokud
je zvěd v čase <i>t</i> na daném políčku, bude na tomto políčku i v čase
<i>t+kp</i>, kde <i>p</i> je délka jeho trasy, pro všechna celočíselná <i>k</i>.

Z toho plyne, že po konečném počtu kroků se všichni zvědové z hradu dostanou
zpátky na své startovní pozice. Tomuto počtu kroků budeme říkat *perioda* a
je roven nejmenšímu společnému násobku délek tras všech zvědů v
hradu.<sup><a id="fl1" href="#fn1">1</a></sup>

Pro hrad s periodou <i>p</i> tedy stačí vygenerovat prvních <i>p</i> „řezů“. Pro
čas <i>t</i> získáme číslo příslušného řezu (*offset*) ze zbytku po dělení <i>t
÷ p</i> (označíme-li počáteční stav časem <i>t = 0</i>). 

Jednotlivé „řezy“ pro jednoduchou mapu jsou zobrazeny na [obrázku
3.1](#img-3.1).

    newtype Slice = Slice (Array Loc SliceField) deriving Show
    data SliceField = FreeSF | WallSF | ScoutSF [Loc] deriving (Eq,Show) 

Typ `Slice` je jen „obalený“ typ `Array Loc SliceField` (tzv.  *newtype*). Od
typového synonymu (uvozeného klíčovým slovem `type`) se liší tím, že jej musíme
explicitně „rozbalovat“ pomocí konstruktoru `Slice` a že typ `Slice` není
ekvivalentní typu `Array Loc SliceField`, můžeme pro něj tedy definovat např.
jiné typové třídy.<sup><a id="fl2" href="#fn2">2</a></sup> Je tedy velmi podobný
klasickému datovému typu (klíčové slovo `data`) s jediným datovým konstruktorem
s jediným prvkem.

`SliceField` je obdobou typu `Field`, rozdíl je v tom, že ve `SliceField`
je možno uložit i zvěda. Parametr typu `[Loc]` konstruktoru `ScoutSF` je
následující pozice zvědů nacházejících se na tomto políčku; využijeme ji při
zjišťování, zda si bílá paní a zvěd v jednom kroku nevyměnili pozice, což je
považováno za objevení bílé paní zvědem.

### 3.4.3 Řezání hradu

Pro samotné rozřezání časoprostoru hradu na tenké plátky definujeme funkci
`sliceCastle`.

    sliceCastle :: Castle -> [Slice]
    sliceCastle castle = map slice [0..period-1] where
      fields = castleFields castle
      period = foldl lcm 1 . map length . castleScouts $ castle
      cycledScouts = map cycle $ castleScouts castle
      sfFields = fmap fieldToSF fields

      slice s = Slice $ accum acc sfFields
        [((x,y),(nextx,nexty)) 
        | scout <- cycledScouts
        , let (x,y):(nextx,nexty):_ = drop s scout
        , x >= 1 && x <= width
        , y >= 1 && y <= height
        ]

      acc :: SliceField -> Loc -> SliceField
      acc old scoutNext = case old of
        FreeSF -> ScoutSF [scoutNext]
        WallSF -> ScoutSF [scoutNext]
        ScoutSF scoutNexts' -> ScoutSF $ scoutNext:scoutNexts'

      fieldToSF Free = FreeSF
      fieldToSF Wall = WallSF

      ((1,1),(width,height)) = bounds $ castleFields castle

Pro přehlednost bude vhodné rozebrat si jednotlivé proměnné:

* `fields` je pole políček (`Array Loc Field`) získané z předaného
hradu.

* `period` je délka periody, s jakou se pohybují všichni zvědové. Jedná
se o nejmenší společný násobek délek tras všech zvědů z hradu. (Funkce @t{lcm a
b} spočte nejvyšší společný násobek dvou čísel `a` a `b`.)

* `cycledScouts` je seznam tras všech zvědů (`[[Loc]]`)
„zacyklených“ do nekonečně se opakujících sekvencí pomocí funkce `cycle`.

* `sfFields` je pole `field` převedené z hodnot typu `Field` na
`FieldSF`. Obsahuje tedy stejnou informaci, jen jiného typu.

* `slice s` je funkce, která pro offset `s` vrátí příslušný řez.
Získáme jej tak, že do „čistého“ pole `sfFields`, obsahujícího pouze
`FreeSF` a `WallSF`, přidáme na příslušné pozice i hodnoty `ScoutSF`, k
čemuž použijeme funkci `accum`. Nesmíme zapomenout zkontrolovat, že se
přidávaný zvěd nachází v hradu, jinak bychom mohli dostat za běhu chybu
(přistupovali bychom k indexu nacházejícímu se mimo rozsah pole).

Typ funkce `accum` je `(e -> a -> e) -> Array i e -> [(i, a)] -> Array i
e`.<sup><a id="fl3" href="#fn3">3</a></sup> První argument je akumulační funkce
<i><code>f</code></i>, druhým výchozí pole <i><code>ary</code></i> a třetí
seznam asociací <i><code>xs</code></i>. Výsledné pole obsahuje stejné prvky jako
<i><code>ary</code></i>, s tím rozdílem, že pro každou asociaci index-hodnota
<i><code>(i,x)</code></i> ze seznamu <i><code>xs</code></i> zavolá funkci
<i><code>f</code></i>, která na základě předchozí hodnoty z pole a hodnoty
<i><code>x</code></i> vrátí novou hodnotu, která se uloží ve výsledném poli na
indexu <i><code>i</code></i>. 

Důležité je, že stejný index se v seznamu <i><code>xs</code></i> může objevit i
vícekrát -- v tom případě se funkci <i><code>f</code></i> předá hodnota získaná
z předchozí asociace. Takto je tedy možno stejný prvek „upravit“ vícekrát.

Je důležité si uvědomit, že pole <i><code>ary</code></i>, předané funkci
`accum`, se nijak nezmění, výsledek se předá v novém poli (jinak to ve
funkcionálním jazyku ani nejde).

V našem případě funkci `accum` předáme pomocnou akumulační funkci `acc`,
pole `sfFields` a seznam vyjádřený pomocí generátoru seznamu, který obsahuje
dvojice `((x,y),(nextx,nexty))`, kde `(x,y)` je pozice zvěda v čase `s` a
`(nextx,nexty)` je pozice stejného zvěda v čase `s+1`. Funkce `accum` tedy
pomocí funkce `acc` na pozici \textit{(x,y)} zapíše informaci o tom, že se zde
nachází zvěd, jehož další destinací je pozice `(nextx,nexty)`.

* `acc old scoutNext` tedy udělá jednu ze dvou věcí: 
    * je-li políčko prázdné (`FreeSF`) nebo zeď (`WallSF`), nahradí jej hodnotou
      `ScoutSF` s jediným zvědem, jehož následující pozice je `scoutNext`; nebo
    * pokud na políčku již byli zapsáni nějací zvědové, přidá k seznamu jejich
      následujících pozic i `scoutNext`.
  \end{inparaenum}

* `fieldToSF` je pomocná funkce, kterou jsme využili v definici pole
`sfFields`.


## 3.5 `Banshee.CastleParser`

Na parsování vstupního souboru s hradem použijeme opět knihovnu `parsec`,
kterou jsme si popsali v sekci [2.5](banshee.html#25_).

    module Banshee.CastleParser(parseCastle) where
    import Banshee.Castle

    import Text.Parsec
    import Control.Applicative ((<$>),(<*),(*>))
    import Control.Monad
    import Data.Array

Funkce `parseCastle` a typ `Parser` jsou definovány obdobně jako v
Krunimírovi:

    parseCastle :: String -> String -> Either ParseError Castle
    parseCastle = parse castle

    type Parser a = Parsec String () a

### 3.5.1 `SemiCastle`

Nejprve nadefinujeme typ `SemiCastle`, který budeme používat v průběhu
parsování. Tento typ představuje „napůl známý“ hrad. Pozice startu a televize
se dozvíme až v průběhu čtení souboru, proto na jejich uložení použijeme typ
`Maybe Loc`. Podobně na uložení políček nepoužijeme pole, ale seznam, jelikož
se informace o jednotlivých políčkách budeme dovídat postupně.

    data SemiCastle = SemiCastle
      { scFields :: [(Loc,Field)]
      , scTV :: Maybe Loc
      , scStart :: Maybe Loc
      , scScouts :: [Scout]
      }

`zeroCastle` je „prázdný“ (nebo nulový) hrad, o kterém zatím nevíme vůbec
nic, a `scAdd` je pomocná funkce, která do předaného `SemiCastle` přidá
jedno políčko.

    zeroCastle = SemiCastle
      { scFields = [] , scTV = Nothing 
      , scStart = Nothing , scScouts = [] }

    scAdd :: Loc -> Field -> SemiCastle -> SemiCastle
    scAdd loc fld sc = sc { scFields = (loc,fld):scFields sc }

### 3.5.2 Jednotlivé parsery

#### Celý hrad

Parser `castle` se stará o parsování celého hradu:

    castle :: Parser Castle
    castle = do
      width <- read <$> (spaces *> many digit)
      height <- read <$> (spaces *> many digit)
      sc <- spaces >> foldM (row width) zeroCastle [1..height]
      eof
      tv <- case scTV sc of
        Just t -> return t
        Nothing -> parserFail "There was no television in the castle"
      start <- case scStart sc of
        Just s -> return s
        Nothing -> parserFail "There was no starting position in the castle"
      return $ Castle 
        { castleFields = array ((1,1),(width,height)) $ scFields sc
        , castleTV = tv, castleStart = start
        , castleScouts = scScouts sc }

Nejprve přečteme šířku (`width`) a výšku (`height`) hradu, která je zapsána
jako dvojice čísel na začátku souboru. Následně pomocí monadického kombinátoru
`foldM` a parseru `row`, který si brzy nadefinujeme, přečteme všechny řádky
ze souboru a získáme tak hodnotu `cs` typu `SemiCastle`, která reprezentuje
přečtený hrad.

Funkce `foldM` má typ `Monad m => (a -> b -> m a) -> a -> [b] -> m a` a je
analogická funkci `foldl`, ovšem pracuje s monádami.

Jakmile máme všechny řádky přečteny, ujistíme se, jestli jsme opravdu přečetli
pozici televizoru (`tv`) a startu (`start`). Pokud ne, pomocí `parserFail`
parsování ukončíme s chybou.  Pokud ano, nic nám už nebrání vytvořit hodnotu
`Castle` a vrátit ji.

#### Řádek

Na parsování jednoho řádku použijeme opět `foldM` a parser `field`, který
přečte jedno políčko z mapy. Jako argumenty funkce `row` musíme předat šířku
mapy, `SemiCastle` který upravujeme a číslo řádku, který čteme.

Na konci řádku by měl být znak konce řádku, náš parser ovšem akceptuje jakékoli
prázdné znaky.

    row :: Int -> SemiCastle -> Int -> Parser SemiCastle
    row width sc y = foldM (field y) sc [1..width] <* spaces

#### Políčko

Nyní už se dostáváme k nejdůležitější části našeho parseru -- čtení jednoho
políčka:

    field :: Int -> SemiCastle -> Int -> Parser SemiCastle
    field y sc x = sc `seq` free <|> wall <|> start <|> tv <|> scout
      where
        free  = char '.' >> return (scAdd (x,y) Free sc)
        wall  = char 'X' >> return (scAdd (x,y) Wall sc)
        start = char '&' >> case scStart sc of
          Nothing -> return $ scAdd (x,y) Free sc { scStart = Just (x,y) }
          Just (x',y') -> parserFail $
            "There is already starting position at " ++ show (x',y')
        tv    = char '#' >> case scTV sc of
          Nothing -> return $ scAdd (x,y) Free sc { scTV = Just (x,y) }
          Just (x',y') -> parserFail $
            "There is already television at " ++ show (x',y')
        scout = char '@' >> do
          moves <- many move
          let locs = if null moves 
                then [(x,y)]
                else applyMoves (x,y) $ init moves
          return $ scAdd (x,y) Free sc { scScouts = locs:scScouts sc }

Před samotným parsováním nejprve pomocí funkce `seq` vynutíme vyhodnocení
proměnné `sc`. Pokud bychom to neudělali, postupně by se během parsování
vytvořil řetěz nevyhodnocených hodnot `SemiCastle`. K jeho vyhodnocení by
došlo až na konci parsování a u velkých hradů by mohlo dojít k přetečení
zásobníku.<sup><a id="fl4" href="#fn4">4</a></sup>

Pro každý typ políčka jsme si nadefinovali vlastní pomocný parser. Prázdná pole
(parser `free`) a zdi (`wall`) jsou jednoduché, pouze do hradu přidáme jedno
pole. 

Narazíme-li na políčko s televizorem (`tv`) nebo startovní pozicí (`start`),
nejprve zkontrolujeme, jestli už jsme televizor či start jednou nepřečetli, a
pokud ano, skončíme parsování s chybou; v opačném případě upravíme odpovídající
část `SemiCastle`.

Posledním typem políčka je zvěd (parser `scout`). Za znakem `@` následuje
několik pohybů, ze kterých musíme spočítat zvědovu trasu. Pokud za zvědem není
zadán ani jeden pohyb, jeho trasa je prostá -- stojí stále na své původní
pozici.

Pokud je zadáno pohybů více, pomocí funkce `applyMoves`, kterou si vzápětí
nadefinujeme, pohyby převedeme na seznam pozic. Ze seznamu pohybů ovšem předáme
všechny pohyby kromě posledního (funkcí `init`), protože v posledním kroku
svého cyklu se zvěd vždy přesune na svou počáteční pozici (tuto vlastnost
nekontrolujeme, ale tímto si ji vynutíme).

### 3.5.3 Pohyby

Pro reprezentaci pohybů využijeme jednoduchý algebraický datový typ:
    
    data Move = North | West | South | East deriving (Eq,Show)

Parser `move`, který jsme využili při čtení zvěda, vrací hodnoty tohoto typu
podle přečteného znaku:

    move :: Parser Move
    move = char '^' *> return North
      <|> char '<' *> return West
      <|> char 'v' *> return South
      <|> char '>' *> return East

Funkce `applyMoves` „aplikuje“ seznam pohybů na počáteční pozici, čímž
dostaneme seznam pozic:

    applyMoves :: Loc -> [Move] -> [Loc]
    applyMoves (x,y) []     = (x,y):[]
    applyMoves (x,y) (m:ms) = (x,y):case m of
      North -> applyMoves (x,y-1) ms
      West  -> applyMoves (x-1,y) ms
      South -> applyMoves (x,y+1) ms
      East  -> applyMoves (x+1,y) ms


## 3.6 `Banshee.Navigate`

    module Banshee.Navigate(navigate) where
    import Data.Array
    import Data.Array.ST
    import Data.Maybe
    import Control.Monad
    import Control.Applicative
    import Control.Monad.ST

    import Banshee.Castle

### 3.6.1 Popis algoritmu

Náš algoritmus hledání cesty je založen na prohledávání všech možných cest do
šířky s počátkem na startovní pozici bílé paní. O cestě hradem můžeme uvažovat
jako o sérii změny „časopozic“, kde časopozicí budeme nazývat dvojici *pozice* v
hradu a *času*, potřebného k cestě na tuto pozici modulo perioda hradu (offset).
V hradu je tedy celkem <i>w &times; h &times; p</i> časopozic, kde <i>w</i> je
šířka hradu, <i>h</i> je výška a <i>p</i> je perioda. Nejkratší cesta nikdy
neprochází stejnou časopozici dvakrát, jelikož by obsahovala cyklus.

V průběhu výpočtu si budeme udržovat pole, jež pro každou časopozici obsahuje
nejkratší cestu, jak této časopozice dosáhnout.  Pokud v každém kroku algoritmu
přiřadíme jedné časopozici nejkratší cestu, dostaneme nejhorší časovou složitost
<i>O(whp)</i>, tedy přímo úměrnou počtu políček v hradu a periodě.

Ve skutečnosti vždy provedeme řádově méně kroků než <i>whp</i>, jelikož v
hradech bez větších překážek nalezneme cestu rychle, v hradech s více překážkami
sice cestu nalezneme po delší době, ale velká část časopozic bude nedostupných.

### 3.6.2 Typ `Path`

Během výpočtu budeme často manipulovat s cestami. Abychom nemuseli stále dokola
počítat délky cest pomocí `length`, budeme používat typ `Path`, který
obsahuje seznam pozic, kterými cesta vede, spolu s její délkou (tedy počtem
těchto pozic).

    data Path = Path Int [Loc] deriving Show
    pathLength (Path len _) = len

### 3.6.3 Určení možných pohybů z políčka

Funkce `moves` slouží k určení všech možných sousedních pozic, na které se
bílá paní může dostat z dané pozice. Tato funkce má typ `moves :: Bool ->
Slice -> Slice -> (Loc,Path) -> [(Loc,Path)]`:

* První argument (typu `Bool`) určuje, jestli můžeme procházet zdmi.
* Druhý argument (`Slice`) je řez hradu v čase, ze kterého vycházíme.
* Třetí argument (`Slice`) je řez hradu v dalším kroku (v čase, ve kterém
dorazíme na další políčko).
* Poslední argument (`(Loc,Path)`) je dvojice -- pozice, ze které
vycházíme, a cesta, kterou jsme se na tuto pozici dostali.
* Výsledkem (`[(Loc,Path)]`) je seznam pozic, kam se můžeme dostat, spolu
s příslušnými cestami.

Její kód je následující:

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

Funkci jsme implementovali pomocí generátoru seznamu (*list
comprehension*). Nejprve si vygenerujeme dvojice `(tox,toy)` všech čtyř pozic
sousedících s výchozí pozicí `(x,y)`.  Následně zkontrolujeme, jestli se tyto
pozice nachází v hradu, a podíváme se, co nás na této pozici v hradu čeká.

Je-li to volné políčko, je vše v pořádku. Pokud narazíme na zeď, tak pokračujeme
pouze pokud má argument `thruWalls` hodnotu `True`. Pokud by se bílá paní
dostala na stejné políčko se zvědem, musíme tuto pozici přeskočit.

Nakonec zkontrolujeme, jestli se v čase, ve kterém jsme vyšli, na políčku, na
které jdeme, nevyskytují zvědi. Pokud ano, a některý z těchto zvědů se v dalším
tahu přesune na pole, ze kterého jsme vyšli, znamená to, že si bílá paní se
zvědem prohodí místo, což znamená, že by ji zvěd objevil a do této pozice tedy
nemůžeme.

### 3.6.4 Monáda `ST`

V našem algoritmu budeme pracovat s polem, ve kterém si budeme ukládat nejlepší
cesty, které jsme nalezli do každé časopozice v hradě. Mohli bychom použít
klasické pole `Array`, ale protože do tohoto pole budeme zapisovat po jednom
prvku, byl by program velmi neefektivní, jelikož při každém zápisu se celé pole
musí zkopírovat.

Proto využijeme monádu `ST s` <a href="bib.html#b12" class="cite">[12]</a>.
Tento typ je podobný typu `IO` v tom, že umožňuje akcím využívat měnitelné
datové struktury, narozdíl od `IO` jsou však akce v monádě `ST s` omezeny pouze
na „vnitřní“ stav, nemohou tedy např. přistupovat k disku nebo vypisovat znaky
na obrazovku, pouze pracovat s měnitelnou pamětí. To ale znamená, že každá
taková akce je *deterministická* a monádu `ST s` tedy můžeme spustit a získat
její výsledek v čistém kódu.

Na akci typu `ST s a` je tedy možno pohlížet jako na výpočet, jenž využívá
interní stav a jehož výstup má typ `a`. Proměnná `s` slouží k zajištění, že
tento interní stav „neunikne“ z monády `ST`.  Funkce, která nám umožní provést
výpočet v monádě `ST` a získat jeho výstup se jmenuje `runST` a má typ `(forall
s. ST s a) -> a`.<sup><a id="fl5" href="#fn5">5</a></sup>

#### Měnitelné pole `STArray`

Měnitelné pole, které můžeme použít v monádě `ST s`, má typ `STArray s i e`,
kde `s` je stejná typová proměnná, kterou předáme do `ST s`, `i` je typ
indexu a `e` typ prvků.

S takovýmto polem můžeme provádět následující operace:

* `getBounds :: STArray s i e -> ST s (i,i)` <br>
`getBounds ary` získá rozsah indexů pole `ary`.

* `newArray :: (i,i) -> e -> ST s (STArray s i e)` <br>
`newArray (a,b) x` vytvoří nové pole s rozsahem indexů od `a` do `b`,
jehož prvky jsou inicializované na hodnotu `x`.

* `readArray :: STArray s i e -> i -> ST s e` <br>
`readArray ary i` přečte hodnotu prvku na indexu `i` v poli `ary`.

* `writeArray :: STArray s i e -> i -> e -> ST s ()` <br>
`writeArray ary i x` zapíše hodnotu `x` do indexu `i` v poli `ary`.

### 3.6.5 Hledání cest v souvislých oblastech bez průchodu zdí

Nyní si implementujeme funkci `flood`, která pro daný seznam výchozích pozic s
příslušnými cestami nalezne nejlepší cestu do každé časopozice hradu, kam se
můžeme dostat bez průchodu zdí. Tyto nejlepší cesty zapíše do předaného
měnitelného pole `STArray` a vrátí buď cestu k televizoru, pokud byla takováto
nalezena, nebo seznam dosažených pozic s příslušnými cestami.

Pro předaný seznam výchozích pozic a cest musí platit, že všechny cesty prochází
stejným počtem zdí a jsou seřazeny vzestupně podle délky, což znamená, že lepší
cesta vždy v tomto seznamu předchází cestu horší.

Jednotlivé argumenty této funkce jsou následující:

* První argument (typ `Castle`) je hrad, ve kterém hledáme cestu.

* Druhý argument (`[Slice]`) je seznam řezů, na které je tento hrad
„nakrájen“.

* Třetí argument (`STArray s (Int,Loc) (Maybe Path)`) je měnitelné pole
`bests`, které každé časopozici přiřazuje nejlepší cestu, pokud taková
existuje.

* Poslední argument (`[(Loc,Path)]`) je seznam startovních pozic
seřazených podle délky (tyto ještě nejsou zapsány v poli `bests`).

* Výsledek (`ST s (Either Path [(Loc,Path)])`) je výpočet v monádě `{ST
s`, jehož výsledek je buď cesta k televizoru (hodnota `Left`), nebo seznam
pozic a cest, jež byly přidány do pole `bests`, seřazených podle délky.

<span></span>

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
          Nothing -> fmap (starts'++) <$>
            step (t+1) (slice2:slices') (concat nextss ++ rest)
          Just path -> return $ Left path

      period = length slices

Téměř všechnu práci v této funkci vykoná pomocná funkce `step`. Jejím prvním
argumentem je čas `t`, ve kterém má začít, druhým nekonečný seznam řezů hradu
(prvním prvkem je řez v čase `t` -- `slice1`, druhým řez v čase `t+1` --
`slice2`) a posledním seznam dvojic pozic a cest do těchto pozic, ze kterých
hledáme cesty (`locpaths`).

Pokud jsme dostali tento seznam prázdný, funkce `step` jednoduše vrátí prázdný
seznam značící, že televizor nebyl nalezen a do pole `bests` nebylo přidáno
nic.

V opačném případě si nejprve seznam `locpaths` rozdělíme na dvě části --
seznam `starts` obsahuje všechny páry pozice-cesta ze kterých vyjdeme v tomto
kroku, v seznamu `rest` je zbytek, který zpracujeme v dalších krocích.

Následně pro každou dvojici ze seznamu `starts` zkontrolujeme, jestli v poli
`bests` není náhodou již nalezena nějaká jiná cesta, která je tedy lepší.
Pokud ne, do pole zapíšeme tuto cestu a vrátíme jak tuto dvojici, tak seznam
sousedních políček, „zabalené“ v konstruktoru `Just`. Pokud již byla
nalezena lepší cesta, vrátíme `Nothing`.

Tímto získáme seznam `[Maybe ((Loc,Path),[(Loc,Path)])]`, který následně
pomocí složené funkce `unzip . catMaybes` převedeme na
`([(Loc,Path)],[[(Loc,Path)]])`, tedy dvojici, jejíž první prvek je seznam
pozic a cest, které byly uznány za nejlepší (tento seznam označíme jako
`starts'`), a seznam seznamů políček a cest k nim, na které se z těchto
nejlepších cest můžeme dostat (v proměnné `nextss`).

Pak zkontrolujeme, jestli jsme neobjevili nejkratší cestu k televizoru. Pokud
ano, tak tuto cestu vrátíme jako `Left`, jinak rekurzivně zavoláme `step`
znovu a pomocí funkce `fmap` k jejímu výsledku přidáme ještě seznam
`starts'`.

### 3.6.6 Hledání cest včetně procházení zdí

Jako poslední implementujeme vlastní funkci `navigate`. Budeme jí předávat
hrad, seznam řezů tohoto hradu a hodnotu `Bool` značící, jestli povolíme
procházení zdí.

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

Veškerou práci ve funkci `navigate` provedeme v monádě `ST s`, kterou
pomocí `runST` „spustíme“ a získáme výsledek. Poté, co zjistíme šířku a
výšku hradu a nadefinujeme seznam startovních pozic (`start`), vytvoříme pole
`bests`. Veškeré prvky tohoto pole budou nastaveny na hodnotu `Nothing`.

Pokud nemáme povoleno procházet skrz zdi (argument `thruWalls` má hodnotu
`False`), použijeme funkci `flood`, jelikož ta nikdy negeneruje cesty, které
prochází zdmi. V opačném případě zavoláme pomocnou funkci `wallStep`, jež
najde i cesty skrz zdi a kterou si vzápětí popíšeme.

Ať už jsme použili `flood` nebo `wallStep`, získali jsme výsledek typu
`Either Path [(Loc,Path)]` v proměnné `result`. Pokud jsme dostali `Left`,
byla nalezena cesta k televizi. K seznamu pozic z této cesty ještě přidáme
pozici televizoru a cestu otočíme, aby byla pozice startu na prvním místě a
televize na místě posledním. Hodnota `Right` značí, že cesta k televizoru
nebyla nalezena; v tom případě vrátíme `Nothing`.

#### Pomocná funkce `wallStep`

Funkci `wallStep` předáme jako argument pole `bests` a seznam startovních
pozic a cest. Nejprve zavoláme funkci `flood` a její výsledek přiřadíme
proměnné `result`. Pokud nalezla cestu k televizoru, jednoduše ji vrátíme.
Pokud takovou cestu nenalezla a zároveň nenašla cestu ani do jedné nové pozice v
hradě, znamená to, že už žádnou novou cestu nalézt nemůžeme a proto vrátíme
hodnotu `Right`, značící neúspěch.

Pokud ale funkce `flood` nalezla cestu do nových pozic v hradě, seznam dvojic
těchto pozic a cest do nich uložíme do proměnné `locpaths'`. Následně každou z
těchto pozic projdeme a pomocí funkce `moves` (jíž jako první argument
předáme `True`) získáme seznam pozic, do kterých se z nich můžeme jedním
pohybem i skrz zeď dostat. Proměnná `nexts` tedy obsahuje seznam pozic a cest,
které můžou mít o jednu zeď v cestě více. S tímto seznamem poté znovu rekurzivně
zavoláme `wallStep`, která nyní najde i cesty za právě prošlými zdmi.


## 3.7 `Banshee.Main`

Modul `Banshee.Main` obsahuje uživatelské rozhraní programu. Podobně jako u
Krunimíra budeme náš program spouštět v terminálu, ale umožníme uživateli pomocí
argumentů zadaných na příkazovém řádku měnit chování programu.

### 3.7.1 Popis použití programu

Programu na příkazové řádce předáme jméno vstupního souboru s hradem a navíc
můžeme předat některé z následujících přepínačů:

* `-h`, `-?` nebo `-{`-help}] zobrazí návod na použití programu a
skončí.

* `-n` nebo `--not-through` zakáže procházení zdmi (ve výchozím
nastavení je procházení zdmi povoleno).

* `-s` nebo `--show-castle` vypíše předaný hrad se zvýrazněnou
nalezenou cestou (výchozí chování).

* `-q` nebo `--quiet` způsobí, že se vypíše pouze jeden řádek s počtem
kroků cesty a počtem zdí, kterými cesta vede (hrad ani zvýrazněná cesta se
nevypisuje).

* `-i` nebo `--interactive` po nalezení cesty zobrazí textové rozhraní (založené
  na unixové knihovně *ncurses* <a href="bib.html#b7" class="cite">[7]</a>),
  které umožní interaktivně zobrazit průchod bílé paní hradem po nalezené
  nekratší cestě, včetně pohybů zvědů.

* `-j` nebo `--json` zobrazí podobný výstup jako `--quiet`, pouze ve stojově
  čitelném formátu JSON <a href="bib.html#b5" class="cite">[5]</a> (využívá se
  při automatickém testování programu).

### 3.7.2 Zpracování argumentů z příkazové řádky

Na zpracování argumentů předaných na příkazové řádce použijeme modul
`System.Console.GetOpt`, který je součástí standardní knihovny.

Začneme hlavičkou modulu:

    module Banshee.Main(main) where
    import System.IO
    import System.Environment
    import System.Exit
    import System.Console.GetOpt
    import Control.Applicative
    import Control.Monad
    import Data.Array
    import Data.List (intersect)

    import Banshee.Castle
    import Banshee.CastleParser (parseCastle)
    import Banshee.Navigate (navigate)
    import Banshee.Interactive (showInteractive)

#### Definice přepínačů

Nyní si deklarujeme jednoduchý datový typ `Flag`, který reprezentuje
jednotlivé přepínače, které můžeme dostat na příkazové řádce:

    data Flag
      = HelpFlag
      | NotThroughFlag
      | ShowCastleFlag
      | QuietFlag
      | InteractiveFlag
      | JsonFlag
      deriving (Eq,Show)

V seznamu `options` uvedeme seznam všech možných nastavení s krátkými popisy:

    options =
      [ Option ['h','?'] ["help"] (NoArg HelpFlag) 
        "show the help"
      , Option ['n'] ["not-through"] (NoArg NotThroughFlag)
        "disable going through the walls"
      , Option ['s'] ["show-castle"] (NoArg ShowCastleFlag)
        "show the castle with highlighted path (default)"
      , Option ['q'] ["quiet"] (NoArg QuietFlag)
        "show just the minimal information about the found path"
      , Option ['i'] ["interactive"] (NoArg InteractiveFlag)
        "display a minimal interactive UI to show the found path"
      , Option ['j'] ["json"] (NoArg JsonFlag)
        "show the result as JSON"
      ]

Funkce `header` vytvoří hlavičku návodu na použití. Její argument `progname`
je jméno programu (ve většině případů to bude `"banshee"`).

    header progname = "Usage: " ++ progname ++ " [flags] castle-file"

#### Čtení předaných přepínačů

Na začátku `main` získáme seznam předaných argumentů z příkazové řádky pomocí
`getArgs` a předáme jej funkci `getOpt` z knihovny `GetOpt`. První
argument této funkce je hodnota `Permute`, která značí, že přepínače a jména
souborů mohou být na příkazové řádce uvedeny v libovolném pořadí, druhým
argumentem je seznam `options`, třetím je výsledek funkce `getArgs`.

    main :: IO ()
    main = do
      (flags,files,errs) <- getOpt Permute options <$> getArgs

Výsledkem funkce `getOpt` je trojice:

* `flags` je seznam předaných přepínačů (`[Flag]`).
* `files` je seznam předaných jmen souborů (`[String]`).
* Pokud uživatel udělal chybu při zadávání přepínačů, v seznamu `errs`
budou chybové hlášky (`[String]`).

#### Kontrola přepínačů

Ještě než se začneme zabývat předanými přepínači, vygenerujeme si návod k
použití, který posléze můžeme zobrazit uživateli v případě, že udělal chybu. K
tomu nám poslouží funkce `usageInfo` z knihovny `GetOpt`.

      progname <- getProgName
      let usage = usageInfo (header progname) options

Nejprve zkontrolujeme, jestli seznam `errs` neobsahuje nějakou chybu. Pokud
ano, vypíšeme návod k použití a všechny chyby, načež program ukončíme.

      if not (null errs) then do
          hPutStrLn stderr usage
          hPutStrLn stderr $ concat errs
          exitFailure
        else return ()

Pokud si uživatel vyžádal nápovědu, zobrazíme ji a skončíme.

      if HelpFlag `elem` flags then do
          putStrLn usage
          exitSuccess
        else return ()

Přepínače `--quiet`, `--interactive`, `--json` a `--show-castle`
se vzájemně vylučují, vždy lze použít nejvýše jeden.

      let exclusiveFlags = [QuietFlag,InteractiveFlag,JsonFlag,ShowCastleFlag]
      if (> 1) . length . intersect flags $ exclusiveFlags then do
          hPutStrLn stderr usage
          hPutStrLn stderr "The flags --quiet, --interactive, --json\
            \ and --show-castle are mutually exclusive (use at most one)"
          exitFailure
        else return ()

Nakonec zkontrolujeme, jestli jsme dostali právě jeden soubor; pokud ne, opět
vypíšeme návod a skončíme.

      inputFile <- case files of
        [file] -> return file
        _ -> do
          hPutStrLn stderr usage
          hPutStrLn stderr "Expected one input file with castle"
          exitFailure

### 3.7.3 Výpočet trasy

Vstupní soubor přečteme a zparsujeme pomocí funkce `parseCastle`. Ta vrátí
`Right` s výsledným hradem v případě úspěchu a `Left` s chybou v případě
neúspěchu.

Přečtený hrad pak funkcí `sliceCastle` rozřežeme do `slices` a výsledek
hledání cesty uložíme do `result`. 

      txt <- readFile inputFile
      castle <- case parseCastle inputFile txt of
          Right c -> return c
          Left err -> do
            hPutStrLn stderr (show err)
            exitFailure

      let slices = sliceCastle castle
          result =  navigate castle slices thruWalls
          thruWalls = NotThroughFlag `notElem` flags

### 3.7.4 Zobrazení výsledku

Do proměnné `ui` na základě předaných přepínačů přiřadíme příslušnou
zobrazovací funkci a vzápětí ji zavoláme s výsledkem hledání cesty, čímž končí
akce `main`. Jednotlivé zobrazovací funkce popíšeme v následujících
podsekcích.

      let ui :: Maybe [Loc] -> IO ()
          ui | QuietFlag `elem` flags       = showQuiet castle
             | InteractiveFlag `elem` flags = showInteractive castle slices
             | JsonFlag `elem` flags        = showJson castle
             | otherwise                    = showCastle castle
      ui result

### 3.7.5 Tiché zobrazení

Funkce `showQuiet`, kterou použijeme u přepínače `--quiet`, jednoduše
vypíše jeden řádek v závislosti na tom, jesli byla cesta nalezena nebo ne.

    showQuiet :: Castle -> Maybe [Loc] -> IO ()
    showQuiet _ Nothing = 
      putStrLn "No path found"
    showQuiet castle (Just locs) = 
      putStrLn . concat $ ["Found a path with ",show $ length locs," steps",
        " (",show $ countWalls castle locs," through walls)"]

Zde si také nadefinujeme funkci `countWalls`, která spočítá zdi nacházející se
na dané cestě a kterou použijeme i v dalších funkcích.

    countWalls :: Castle -> [Loc] -> Int
    countWalls castle locs =
      length $ filter ((==Wall) . (castleFields castle !)) locs

### 3.7.6 Zobrazení ve formátu JSON

Výsledek ve formátu JSON zobrazíme podobně jako ve funkci `showQuiet`, pouze
vypíšeme objekt ve tvaru <code>{ "steps": <i>počet_kroků</i>, "walls":
<i>počet_zdí</i> }</code> pokud byla cesta nalezena nebo `{}` pokud nebyla.

    showJson :: Castle -> Maybe [Loc] -> IO ()
    showJson _ Nothing =
      putStrLn "{}"
    showJson castle (Just locs) = 
      putStrLn . concat $ ["{ \"steps\": ",show $ length locs,
        ", \"walls\": ",show $ countWalls castle locs," }"]

### 3.7.7 Zobrazení cesty v hradu

Funkce `showCastle` vypíše celý hrad a vyznačí v něm nalezenou trasu,
samozřejmě pouze pokud byla nějaká cesta nalezena. Zdi jsou zaznačeny znaky
`X`, volná políčka znaky `.`. Cestu značíme znaky `+`, cestu skrz
zeď znakem `~'.

    showCastle :: Castle -> Maybe [Loc] -> IO ()
    showCastle _ Nothing = 
      putStrLn "No path found"
    showCastle castle (Just locs) = do
      forM_ [1..height] $ \y -> do
        forM_ [1..width] $ \x -> do
          putChar $ ary ! (x,y)
        putChar '\n'
      putStrLn . concat $ ["\n",show $ length locs," steps",
        " (",show $ countWalls castle locs," through walls)"]
      where

      ((1,1),(width,height)) = bounds $ castleFields castle

      fieldAry = fieldChar `fmap` castleFields castle
      fieldChar Free = '.'
      fieldChar Wall = 'X'

      ary = fieldAry // [((x,y),pathChar (x,y)) | (x,y) <- locs]
      pathChar (x,y) = case castleFields castle ! (x,y) of
          Free -> '+'
          Wall -> '~'

Vykreslení provádíme tak, že si vytvoříme pole `ary` o rozměrech hradu,
zaplníme jej znaky a pak jej po řádcích vypíšeme.

Nejprve pole políček hradu, získané funkcí `castleFields`, převedeme pomocí
funkce `fmap` na pole znaků `fieldAry`. Následně funkcí `//` změníme ta
políčka, která se nachází na cestě (v seznamu pozic `locs`), na znak `+`
nebo `~` v závislosti na tom, jestli na tomto políčku je zeď
nebo ne.

### 3.7.8 Interaktivní zobrazení

Interaktivní zobrazení zajišťuje funkce `showInteractive` z modulu
`Banshee.Interactive`. Její typ je:

    showInteractive :: Castle -> [Slice] -> Maybe [Loc] -> IO ()

Tato funkce umožňuje krok po kroku procházet nalezenou cestu, včetně
pohybujících se zvědů. Modul `Banshee.Interactive` zde nebudeme přetiskovat,
jelikož je poměrně rozsáhlý (přibližně 180 řádků) a využívá knihovnu `ncurses`
<a href="bib.html#b15" class="cite">[15]</a>, jejíž popis je mimo rozsah této
práce.

## 3.8 Závěr

Zde prezentované řešení soutěžní úlohy je téměř kompletní, podobně jako u
Krunimíra jsme ale vynechali část s grafickým uživatelským rozhraním (editorem
hradu). Na soutěži by nejspíše obdrželo přibližně 60 bodů z 90 (zbývajících 30
je za editor hradu) a možná nějaký bonus za interaktivní režim.

Nejdůležitější vlastností u podobných úloh je vždy rychlost. V tomto ohledu je
naše řešení velmi dobré -- všechny testovací soubory ze složek
[`test/data_ukazkova/`](https://github.com/honzasp/funsp/tree/master/banshee/test/data_ukazkova)
a
[`test/data_testovaci/`](https://github.com/honzasp/funsp/tree/master/banshee/test/data_testovaci)
se na vývojovém počítači<sup><a id="fl6" href="#fn6">6</a></sup>} vyřeší za
necelé dvě sekundy (z toho asi 0.4&nbsp;s zabere nejnáročnější soubor
`data_testovaci/vstupy/4se_zvedy_skrz_zdi/vstup8.in`), spotřeba paměti se
pohybuje okolo 10&nbsp;MB.

Program je zároveň poměrně krátký, obsahuje méně než 300 řádků kódu (bez modulu
`Banshee.Interactive`), včetně přívětivého uživatelského rozhraní.

### 3.8.1 Zdrojové kódy

Soubory související s úlohou Bílá paní se nachází ve složce
[`banshee/`](https://github.com/honzasp/funsp/tree/master/banshee) v repozitáři
s prací. Zdrojové kódy všech modulů jsou uloženy ve složce
[`banshee/Banshee/`](https://github.com/honzasp/funsp/tree/master/banshee/Banshee),
testovací soubory ve složce
[`banshee/test/`](https://github.com/honzasp/funsp/tree/master/banshee/test).
Část těchto souborů pochází ze soutěže a část byla vytvořena v rámci této práce.
Konkrétně ve složce
[`test/extra/huge/`](https://github.com/honzasp/funsp/tree/master/banshee/test/extra/huge)
se nachází velmi velké hrady a ve složce
[`test/extra/cellular/`](https://github.com/honzasp/funsp/tree/master/banshee/test/extra/cellular)
jsou hrady vytvořené pomocí celulárních automatů.

K automatickému testování korektnosti řešení je možné využít program
[`runtests.hs`](https://github.com/honzasp/funsp/blob/master/banshee/runtests.hs),
který čte názvy souborů s hrady a očekávané výsledky ze souboru
[`test/results.json`](https://github.com/honzasp/funsp/blob/master/banshee/test/results.json)
a kontroluje, jestli nalezená řešení témto očekávaným výsledkům odpovídají.

<div class="footnotes">
  <!-- <sup><a id="fl1" href="#fn1">1</a></sup> -->

  <p id="fn1"><sup><a href="#fl1">1</a></sup> Z původního zadání plyne, že délka
  tras všech zvědů musí být sudá a že nepřesáhne 8, tudíž nejvyšší možná perioda
  je 24. Náš program akceptuje libovolně dlouhé trasy zvědů, periodu tedy budeme
  počítat pro každý hrad zvlášť.</p>

  <p id="fn2"><sup><a href="#fl2">2</a></sup> To je důvod, proč Haskell takovéto
  typy zavádí.</p>

  <p id="fn3"><sup><a href="#fl3">3</a></sup> Pro jednoduchost jsme vynechali
  omezení třídy <code>Ix i => ...</code></p>

  <p id="fn4"><sup><a href="#fl4">4</a></sup> Velkými hrady myslíme hrady s
  milióny políček -- pokud bychom se drželi omezení na 80&times;80 polí, jak je
  uvedeno v zadání, žádný problém by nenastal.</p>

  <p id="fn5"><sup><a href="#fl5">5</a></sup> Kvantifikátor <code>forall</code>
  je součástí typové magie zajišťující bezpečnost použití <code>ST</code> a je
  součástí rozšíření <code>ExistentialQuantification</code> jazyka Haskell.
  </p>
  
  <p id="fn6"><sup><a href="#fl6">6</a></sup>AMD Athlon&reg; 64 X2 Dual Core,
  frekvence procesoru 1000&nbsp;MHz.</p>

</div>


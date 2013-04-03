---
layout: paper
title: "Kapitola 2 &mdash; Krunimír: želví grafika"
---

# {{ page.title }}

> Želvák Krunimír je velký myslitel. Zjistil, ze když si za krunýř přiváže trochu
> křídy, kreslí za sebou při svém plazení cestičku. I pojal plán nakreslit svoji
> podobiznu, samozřejmě včetně přesných detailů krunýře. Hned se dal pln nadšení
> do díla a práce mu šla pěkně od ... tlapy.

> „Co to děláš, dědečku,“ zeptal se jednou Krunimíra jeho vnuk Krunoslav. „Krešlím
> tady švou poďobižnu,“ odpověděl Krunimír. „Žačal jsem š ní, když tvůj tatík
> ještě nebyl na švětě, a ještě nemám ani krunýř,“ dodal smutně. „To už ji aši
> dokrešlit neštihnu...“ Vnuk Krunoslav, znalec moderní techniky, mu však poradil:
> „Tak si nech napsat program, který ji nakreslí za Tebe.“ 
> Protože se ale s tlapami a zobákem moc dobře neprogramuje, najali si želváci
> vás. <a href="bib.html#b2" class="cite">[2]</a>

Takto začíná zadání finální úlohy Soutěže v programování z roku 2010 <a
href="bib.html#b2" class="cite">[2]</a>  (v práci přetisknuto na straně 61).
Popisuje jednoduchý procedurální jazyk na generování želví grafiky, inspirovaný
jazykem Logo, a úkolem je vytvořit interpret tohoto jazyka, jehož vstupem je
text programu a výstupem vykreslený obrázek.

## 2.1 Popis jazyka

Krunimírův jazyk umožňuje vykreslovat obrázky principem želví grafiky. Na
začátku se želva nachází uprostřed obrázku o velikosti 701×701 pixelů vyplněného
bílou barvou a je otočená směrem nahoru. Uživatel má k dispozici několik
primitivních kreslících procedur, kterými může želvu ovládat:

* `forward(d)` -- Želva se posune vpřed o `d` jednotek (pixelů). Pokud je
  tloušťka pera kladná, zanechá za sebou čáru vedoucí z původní pozice do nové.
  Takovýto pohyb se považuje za jeden tah.

* `right(a)`, `left(a)` -- Želva se otočí doprava, resp. doleva o `a` stupňů.

* `pen(s)` -- Nastaví tloušťku pera na `s` pixelů. Je-li tloušťka pera nulová,
  želva nic nekreslí.

* `color(r,g,b)` -- Nastaví barvu pera, `r`, `g` a `b` jsou jednotlivé složky
  barevného modelu RGB v rozsahu 0 až 255.

Jazyk dále umožňuje použít jednoduchou podmínku a cyklus:

* `if(x) { ... }` -- Vykoná příkazy v těle podmínky právě když je `x` kladné.

* `repeat(x) { ... }` -- Vykoná příkazy `x`-krát, je-li `x` kladné.

Uživatel může definovat vlastní procedury a volat je:

<ul>

  <li>
    <code>define <i>procedura</i>(<i>p1</i>,<i>p2</i>,...) { ... }</code> –
    Definuje proceduru <i>procedura</i>, která má libovolný počet parametrů
    (<i>p1</i>, <i>p2</i>, ...). Tyto parametry mohou být v těle procedury
    použity ve výrazech a nabývají hodnoty předané v místě volání.
  </li>

  <li>
    <code><i>procedura</i>(arg1,arg2,...)</code> – Zavolá proceduru
    <i>procedura</i> s argumenty (arg1, arg2, ...). Procedura musí být
    definována <em>před</em> svým voláním a může být rekurzivní.
  </li>

</ul>

Poslední a nejzajímavější struktura je rozdvojení:

* `split { ... }` -- Vytvoří klon aktuální želvy, která vykoná příkazy v těle
  struktury split, přičemž původní želva pokračuje ve vykonávání dalších
  příkazů. Všechny želvy se pohybují paralelně, vždy všechny provedou jeden tah,
  poté druhý atd.

Jako argumenty při volání procedur lze používat výrazy vytvořené z celočíselných
literálů, parametrů aktuální procedury, binárních operátorů `+`, `-`, `*` a `/`
(dělení je celočíselné) a negace pomocí operátoru `-`. Ve výrazech je možno
používat závorky `(` a `)`, priorita a asociativita operátorů je jako v
matematice.

### 2.1.1 Příklady

Uvedeme si několik příkladů, které ilustrují využití veškerých příkazů
Krunimírova jazyka.  Vygenerované výstupy těchto příkladů jsou na <a
href="#img-2.2">obrázku 2.2</a>.

#### Čtverec ([2.2a](#img-2.2-a))

Jednoduchý kód, který vykreslí čtverec.

    pen(1)
    forward(200) right(90)
    forward(200) right(90)
    forward(200) right(90)
    forward(200) right(90)

#### Mřížka čtverců ([2.2b](#img-2.2-b))

V této ukázce využijeme procedury, abychom kód rozdělili na menší a přehlednější
části.

    define square(side) {
      pen(1) repeat(4) { forward(side) right(90) } pen(0)
    }

    define row() {
      repeat(4) { square(114) forward(163) }
      forward(-652) right(90) forward(163) left(90)
    }

    forward(301) left(90) forward(301) right(180)
    repeat(4) { row() }

#### Binární strom ([2.2c](#img-2.2-c))

Při vykreslování stromů prokáže svoji užitečnost příkaz split.

    define tree(n) {
      if(n) {
        forward(40+20*n)
        split { left(5+3*n) tree(n-1) }
        right(5+3*n) tree(n-1)
      }
    }

    forward(-250) pen(1) tree(5)

<figure class="multifigure horizontal" id="img-2.2">
  <figure id="img-2.2-a">
    <div class="image"><img src="../img/square1.svg" width="250"
    height="250"></div>
    <figcaption>(a) Čtverec</figcaption>
  </figure>

  <figure id="img-2.2-b">
    <div class="image"><img src="../img/squares.svg" width="250"
    height="250"></div>
    <figcaption>(b) Mřížka čtverců</figcaption>
  </figure>

  <figure id="img-2.2-c">
    <div class="image"><img src="../img/bintree.svg" width="250"
    height="250"></div>
    <figcaption>(c) Binární strom</figcaption>
  </figure>

  <span class="clear">.</span>

  <figcaption>Obrázek 2.2: Výsledné obrázky z příkladů</figcaption>
</figure>

## 2.2 Analýza

Problém si můžeme rozdělit na tři části:

1. *Syntaktická analýza* („parsování“) zpracuje vstupní řetězec na *abstraktní
   syntaktický strom*, který zachycuje strukturu programu ve formě, která je
   jednoduše zpracovatelná v dalších fázích.

2. Následuje *vyhodnocení*, kdy ze syntaktického stromu vypočteme výslednou
   stopu (ve vektorové podobě jako seznam úseček).

3. Poslední částí je *vykreslení*, které vykreslí vyhodnocenou stopu do obrázku.
   Budeme exportovat do rastrových obrázků formátu PNG a vektorových formátu
   SVG.

Pomocí tohoto jednoduchého rozdělení můžeme naše řešení rozvrhnout do sedmi
modulů:

* `Krunimir.Main` exportuje `main`, která slouží jako rozhraní s
  uživatelem.<sup><a id="fl1" href="#fn1">1</a></sup>

* `Krunimir.Parser` exportuje funkci `parse`, která z textového zápisu programu vytvoří syntaktický strom (nebo syntaktickou chybu, pokud program není korektní).

* `Krunimir.Ast` definuje datové typy, které reprezentují syntaktický strom.

* `Krunimir.Evaluator` poskytuje funkci `eval`, která ze syntaktického stromu
  vypočte výslednou stopu.

* `Krunimir.Trace` definuje datové typy a funkce spojené se stopou želvy.

* `Krunimir.PngRenderer` exportuje funkci `renderPng`, která vykreslí stopu jako
  PNG obrázek.

* `Krunimir.SvgRenderer` poskytuje funkci `renderSvg`, jenž uloží stopu ve
  vektorovém formátu SVG.

Nyní můžeme přejít na popis jednotlivých modulů, ze kterých je složen výsledný program.

## 2.3 `Krunimir.Main`

Začneme modulem `Krunimir.Main`, ve kterém definujeme hodnotu `main`, která
tvoří „tělo“ programu.

    module Krunimir.Main (main) where

Potřebujeme několik funkcí z knihoven

    import System.Environment (getArgs,getProgName)
    import System.IO (stderr,hPutStrLn)
    import System.Exit (exitFailure)
    import System.FilePath (replaceExtension)
    import Data.Time.Clock (getCurrentTime,diffUTCTime)

a také námi definované funkce z ostatních modulů

    import Krunimir.Parser(parse)
    import Krunimir.Evaluator(eval)
    import Krunimir.PngRenderer(renderPng)
    import Krunimir.SvgRenderer(renderSvg)
    import Krunimir.Trace(prune)

    main :: IO ()
    main = do

Uložíme si čas na začátku běhu programu, bude se nám hodit, až budeme chtít
zjistit, jak dlouho výpočet trval.

      startTime <- getCurrentTime

Nejprve se podíváme, jaké argumenty jsme dostali na příkazové řádce, a podle
toho nastavíme proměnnou `inputFile` obsahující jméno vstupního souboru, a
`steps`, což je <code>Just <i>početKroků</i></code>, pokud máme zadaný počet
kroků, nebo `Nothing`, pokud jej zadaný nemáme (takže předpokládáme, že uživatel
chce vykreslit celý obrázek).<sup><a id="fl2" href="#fn2">2</a></sup> Pokud
uživatel zadal jiný počet argumentů než jeden nebo dva, vypíšeme chybu a pomocí
IO operace `exitFailure` ukončíme program s návratovým kódem, který signalizuje
selhání.

      args <- getArgs
      (inputFile,steps) <- case args of
        [file] -> return (file,Nothing)
        [file,steps] -> return (file,Just $ read steps)
        _ -> do
          progname <- getProgName
          hPutStrLn stderr $ "Use: " ++ progname ++ " input-file [steps]"
          exitFailure

Nyní můžeme přečíst požadovaný soubor a jeho obsah předat funkci `parse`
z modulu `Krunimir.Parser`. Pokud dostaneme chybu, zobrazíme ji na
chybový výstup a program přerušíme.

      txt <- readFile inputFile
      ast <- case parse inputFile txt of
        Right ast -> return ast
        Left err -> do
          hPutStrLn stderr $ show err
          exitFailure

Úspěšně přečtený syntaktický strom můžeme předat funkci `eval` a
dostaneme výslednou stopu v písku (`fullTrace`). Pokud uživatel zadal omezení
počtu kroků, pomocí funkce `prune` stopu omezíme, pokud ne, necháme ji
celou (`prunedTrace`).

      let fullTrace = eval ast
          prunedTrace = case steps of
            Nothing -> fullTrace 
            Just count -> prune count fullTrace

Díky línému vyhodnocování se v případě, kdy je počet kroků omezen, vypočítá jen
ta část stopy, která nás zajímá. Máme tedy zajištěno, že i když bude mít stopa
desetitisíce kroků a uživatel bude chtít zobrazit jen prvních několik, nebudeme
počítat celou stopu, ale jen zobrazenou část. Naše implementace dokonce umožňuje
spouštět nekonečné programy, samozřejmě pouze pokud uživatel specifikuje počet
kroků, jež si přeje vykonat.

      let outputPng = replaceExtension inputFile ".test.png"
          outputSvg = replaceExtension inputFile ".test.svg"

Jména výstupních souborů (jak PNG, tak SVG) odvodíme ze jména souboru vstupního,
jen změníme příponu. Zbývá jen vykreslit

      renderPng prunedTrace outputPng
      renderSvg prunedTrace outputSvg

a vypsat řádek, který nás informuje o délce výpočtu:

      endTime <- getCurrentTime
      putStrLn $ show (diffUTCTime endTime startTime) ++ " : " ++ inputFile


## 2.4 `Krunimir.Ast`

V tomto modulu definujeme datové typy popisující syntaktický strom, které
využijeme v modulech `Krunimir.Parser` a `Krunimir.Evaluator`.

    module Krunimir.Ast where

### Definice typů

#### Příkazy

Příkaz je reprezentován datovým typem `Stmt`, který obsahuje konstruktor
pro každý z příkazů želvího jazyka.

    data Stmt = 
        ForwardStmt Expr
      | LeftStmt Expr
      | RightStmt Expr
      | PenStmt Expr
      | ColorStmt Expr Expr Expr
      | RepeatStmt Expr [Stmt]
      | IfStmt Expr [Stmt]
      | SplitStmt [Stmt]
      | CallStmt String [Expr]
      deriving Show

#### Výrazy

Reprezentace výrazů je podobně přímočará:

    data Expr =
        LiteralExpr Integer
      | VariableExpr String
      | BinopExpr Op Expr Expr
      | NegateExpr Expr
      deriving Show

    data Op = AddOp | SubOp | MulOp | DivOp
      deriving Show

#### Definice

Poslední strukturou je definice uživatelské procedury, pro kterou použijeme
datový typ s pojmenovanými prvky (záznam neboli *record*).

  data Define = Define
    { defineName :: String
    , defineParams :: [String]
    , defineStmts :: [Stmt]
    } deriving Show

#### Programy

Na nejvyšší úrovni v programu se mohou nacházet jak definice funkcí, tak
příkazy, což odráží typ `TopStmt`. Těmto strukturám budeme říkat
*top-příkazy*. Želví program je pak jen seznam těchto „top-příkazů“.

    type Program = [TopStmt]

    data TopStmt = TopDefine Define | TopStmt Stmt
      deriving Show

Identifikátor `TopStmt` může označovat dvě odlišné entity -- *typový*
konstruktor `TopStmt` a *datový* konstruktor `TopStmt` příslušející
stejnojmennému typu, podle kontextu je ale vždy jasné, který z těchto dvou
konstruktorů myslíme.  V Haskellu se s takovýmito případy, kdy definujeme datový
typ se stejnojmenným konstruktorem, setkáváme poměrně často.

### Příklad

Využijeme program, který nakreslí binární strom a který jsme již jednou uvedli
(jeho výstup je na obrázku [2.2c](#img-2.2-c)). Pro přehlednost ještě jednou
zopakujeme jeho kód:

    define tree(n) {
      if(n) {
        forward(40+20*n)
        split { left(5+3*n) tree(n-1) }
        right(5+3*n) tree(n-1)
      }
    }

    forward(-250) pen(1) tree(5)

Tento program je reprezentován pomocí výše uvedených typů následovně (text
následující za `--` jsou komentáře):

    [
      -- nejprve definice procedury `tree'
      TopDefine (Define {
        defineName = "tree", -- jméno definované procedury
        defineParams = ["n"], -- seznam parametrů
        defineStmts = [ -- seznam příkazů v těle procedury
          IfStmt (VariableExpr "n") [
            -- příkaz `forward(40+20*n)'
            ForwardStmt (BinopExpr AddOp
              (LiteralExpr 40)
              -- podvýraz `20*n'
              (BinopExpr MulOp (LiteralExpr 20) (VariableExpr "n"))),
            -- příkaz `split \{ left(5+3*n) tree(n-1) \}'
            SplitStmt [
              LeftStmt (BinopExpr AddOp
                (LiteralExpr 5)
                (BinopExpr MulOp (LiteralExpr 3) (VariableExpr "n"))),
              CallStmt "tree" [BinopExpr SubOp (VariableExpr "n") (LiteralExpr 1)]
            ],
            -- příkaz `right(5+3*n)'
            RightStmt (BinopExpr AddOp 
              (LiteralExpr 5) 
              (BinopExpr MulOp (LiteralExpr 3) (VariableExpr "n"))),
            -- příkaz `tree(n-1)'
            CallStmt "tree" [BinopExpr SubOp (VariableExpr "n") (LiteralExpr 1)]
          ]
        ]
      }),
      -- trojice příkazů následujících za definicí procedury
      TopStmt (ForwardStmt (NegateExpr (LiteralExpr 250))),
      TopStmt (PenStmt (LiteralExpr 1)),
      TopStmt (CallStmt "tree" [LiteralExpr 5])
    ]

## 2.5 `Krunimir.Parser`

Pro syntaktickou analýzu („parsování“) použijeme knihovnu `parsec` <a
href="bib.html#b12" class="cite">[13]</a>. Jedná se o jeden z nejpoužívanějších
nástrojů na tvorbu parserů v Haskellu.

    module Krunimir.Parser (Krunimir.Parser.parse) where
    import Text.Parsec 
    import Control.Applicative ((<$>), (<$), (<*), (*>), (<*>))
    import Krunimir.Ast

Narozdíl od generátorů jako GNU Bison <a href="bib.html#b8"
class="cite">[8]</a>, které vyžadují speciální soubor s definicí gramatiky a
strojově jej překládají do cílového jazyka, se `parsec` používá v normálním kódu
Haskellu a parsery se konstruují pomocí *vysokoúrovňových kombinátorů*, které
umožňují kombinovat malé parsery do větších celků.

Jako formální základ pro náš parser použijeme gramatiku PEG <a
href="bib.html#b6" class="cite">[6]</a>. Výhodou PEG gramatik je jejich snadná
implementace, jelikož popisují *rozpoznávání* jazyka, narozdíl od tradičních
bezkontextových gramatik, které byly vytvořeny pro popis lidských jazyků a
definují jejich *generování*. 

### Základní definice

Parsery v knihovně `parsec` mají typ `Parsec s u a`, kde:

* `s` je typ vstupu, v našem případě `String`.
* `u` je typ uživatelského stavu, tj. data, která uživatel
  (programátor parserů) může ukládat během parsování. My tuto vlastnost
  využívat nebudeme, proto použijeme „prázdný“ typ `()`.
* `a` je výsledek parseru, tedy typ který parser vrátí. Naše parsery
  budou vracet více typů, např. příkaz (`Stmt`) nebo číslo
  (`Integer`).

Abychom nemuseli neustále opakovat `Parsec String () ...`, nadefinujeme
*typový synonym*, který využijeme i při prezentaci jednotlivých
kombinátorů.

    type Parser a = Parsec String () a

### Představení základních kombinátorů

Některé kombinátory definuje přímo `parsec`:

* `char :: Char -> Parser Char` <br>
  `char c` vytvoří parser, který akceptuje znak `c` a v případě úspěchu jej
  vrátí.

* `string :: [Char] -> Parser [Char]` <br>
  `string cs` je parser, jenž akceptuje sekvenci znaků (řetězec) `cs`.

* `(<|>) :: Parser a -> Parser a -> Parser a` <br>
  `p <|> q` představuje volbu -- nejprve aplikuje parser `p`, a pokud selže,
  *aniž zkonzumoval nějaký vstup*, aplikuje parser `q`.

* `(<?>) :: Parser a -> String -> Parser a` <br>
  `p <?> msg` aplikuje parser `p`, a pokud selže, \emph{aniž zkonzumoval
  část vstupu}, nahradí část `"Expected ..."` chybové zprávy řetězcem `msg`.

* `try :: Parser a -> Parser a` <br>
  `try p` funguje jako `p`, ale s tím rozdílem, že pokud `p` selže,
  předstírá, že nic nezkonzumoval. Použijeme ho nejčastěji ve spojení s `<|>`.

* `many :: Parser a -> Parser [a]` <br>
  `many p` aplikuje parser `p` *nula* či vícekrát a vrátí seznam
  výsledků z `p` (což znamená, že pokud `p` poprvé skončí neúspěchem,
  `many` vrátí prázdný seznam).

* `many1 :: Parser a -> Parser [a]` <br>
  `many1 p` funguje obdobně jako `many p`, s tím rozdílem, že `p` aplikuje
  *vždy alespoň jednou* (pokud `p` napoprvé selže, skončí neúspěchem i
  `many1`).

* `sepBy :: Parser a -> Parser sep -> Parser [a]` <br> `p `sepBy` s` zparsuje
  *nula* či více výskytů `p` oddělených `s`.<sup><a id="fl3"
  href="#fn3">3</a></sup> Obdobně jako u `many` existuje varianta `sepBy1`,
  která aplikuje `p` alespoň jednou.


Každý parser je samozřejmě *monáda*, proto můžeme použít základní monadické
operace:

* `(>>=) :: Parser a -> (a -> Parser b) -> Parser b` <br>
  `p >>= f` aplikuje parser `p` a jeho výsledek předá funkci `f`.

* `(>>) :: Parser a -> Parser b -> Parser a` <br>
  `p >> q` nejprve aplikuje parser `p`, jeho výsledek zahodí a aplikuje
  `q`.

* `return :: a -> Parser a` <br>
  `return x` vytvoří parser, který vždy uspěje a vrátí `x`.

Každá monáda je *aplikativní funktor*, tudíž můžeme použít i následující
funkce:

* `(<\$>) :: (a -> b) -> Parser a -> Parser b` <br>
  `f <\$> p` aplikuje parser `p` a v případě úspěchu předá jeho výsledek
  funkci `f`, jejíž výstup se stane výsledkem `<\$>`.

* `(<*>) :: Parser (a -> b) -> Parser a -> Parser b` <br>
  `p <*> q` nejprve aplikuje `p`, poté `q` a výsledek z `q` předá funkci
  získané z `p`, jejíž výstup se stane výsledkem `<*>`.

* `(<\$) :: a -> Parser b -> Parser a` <br>
  `x <\$ p` aplikuje parser `p`, ale jeho výsledek zahodí a namísto toho
  vrátí `x`.

* `(<*) :: Parser a -> Parser b -> Parser a` <br>
  `p <* q` aplikuje nejprve parser `p`, poté parser `q`,
  jehož výsledek zahodí a vrátí výsledek `p`.

* `(*>) :: Parser a -> Parser b -> Parser b` <br>
  `p *> q` aplikuje parser `p`, poté `q`, jehož výsledek
  vrátí. Tato funkce je ekvivalentní s `>>`, ale použití spolu s
  `<*` dáme přednost této variantě.


### Funkce `parse`

Funkce `parse` představuje „rozhraní“ modulu `Krunimir.Parser`. Vstupem
je jméno parsovaného souboru (použije se v případných chybových hláškách) a
samotný text programu. Výstupem je buď chyba (`ParseError`) nebo želví program
(`Program`).

Využijeme stejně pojmenovanou funkci, kterou nám `parsec` nabízí, a
předáme jí nejprve parser celého programu (`program`) a pak oba zbývající
argumenty.

    parse :: String -> String -> Either ParseError Program
    parse filename txt =
      Text.Parsec.parse program filename txt

### Programy

Na začátku programu může být libovolné množství prázdných znaků, následuje nula
a více top-příkazů a konec souboru.

    program :: Parser Program
    program = spaces *> many topStmt <* eof

Operátory `*>` a `<*` mají stejnou prioritu a jsou asociativní zleva, což
znamená že tento kód je ekvivalentní `(spaces *> many topStmt) <* eof`.  Nejprve
se tedy aplikuje `spaces`, <sup><a id="fl4" href="#fn4">4</a></sup> jehož
výsledek se zahodí, poté `many topStmt`, kterým získáme seznam top-příkazů, a
nakonec `eof`. Pokud `eof` uspěje, dostaneme výsledek z `many topStmt`, pokud
ne, parser vrátí chybu.

#### Top-příkazy

Top-příkaz je buď definice procedury (parser `define`) nebo příkaz (parser
`stmt`), ze kterých pomocí příslušných datových konstruktorů (`TopDefine`,
resp. `TopStmt`) vytvoříme typ `TopStmt`.

    topStmt :: Parser TopStmt
    topStmt = 
      TopDefine <$> try define <|>
      TopStmt <$> stmt

#### Definice procedur

Definice procedur v Krunimírově jazyku začínají klíčovým slovem `define`
následovaným jménem procedury, za kterým je v závorkách nula a více parametrů.
Tělo procedury je uzavřeno ve složených závorkách.

    define :: Parser Define
    define = do
      string "define" >> skipMany1 space
      name <- identifier
      params <- parens $ identifier `sepBy` comma
      stmts <- braces $ many stmt
      return $ Define name params stmts

Použili jsme pomocné funkce `parens` a `braces`, které slouží k
„obalování závorkami“ a které si nadefinujeme později.

### Příkazy

K parsování *příkazů* slouží `stmt`, která jen aplikuje další
pomocné parsery a pojmenuje případnou chybu.

    stmt :: Parser Stmt
    stmt =
      try repeatStmt <|>
      try ifStmt <|>
      try splitStmt <|>
      try procStmt <?>
      "statement"

#### Volání procedur

Začneme syntaxí užitou při volání procedur. Jak zabudované primitivní
(`forward`, `color`...), tak programátorem definované procedury se volají
syntaktivky stejně, proto je musíme rozlišit podle jména a podle toho vytvořit
příslušný uzel syntaktického stromu.

Volání začíná jménem volané procedury a následuje v závorkách seznam argumentů,
který může být prázdný, závorky ale vynechat nelze.

    procStmt :: Parser Stmt
    procStmt = do
      name <- identifier
      args <- parens $ expr `sepBy` comma
      case name of
        "forward" -> primitive ForwardStmt "forward" args
        "left"    -> primitive LeftStmt "left" args
        "right"   -> primitive RightStmt "right" args
        "pen"     -> primitive PenStmt "pen" args
        "color"   -> case args of
            [r,g,b] -> return $ ColorStmt r g b
            _       -> parserFail $
                "color takes 3 arguments, got " ++ show (length args)
        _ -> return $ CallStmt name args
      where
        primitive con _ [arg] = return $ con arg
        primitive _ name args = parserFail $
            name ++ " takes 1 argument, got " ++ show (length args)

Využili jsme parsery `identifier` a `parens`, které si
nadefinujeme později, a pomocnou funkci `primitive`, kterou si ušetříme
opakování při zpracování příkazů `forward`, `left`, `right`
a `pen`, které všechny vyžadují jeden argument.

#### Příkazy `if` a `repeat`

Syntaxe pro `if` a `repeat` je velmi podobná -- nejprve klíčové
slovo, poté v závorkách výraz a nakonec seznam příkazů ve složených závorkách.

    repeatStmt :: Parser Stmt
    repeatStmt = do
      keyword "repeat"
      times <- parens expr
      stmts <- braces $ many stmt
      return $ RepeatStmt times stmts

    ifStmt :: Parser Stmt
    ifStmt = do
      keyword "if"
      cond <- parens expr
      stmts <- braces $ many stmt
      return $ IfStmt cond stmts

Pomocný parser `keyword` nadefinujeme později; kdybychom místo něj použili
jednoduše `string`, například `string "if"`, a programátor by nadefinoval
třeba proceduru `iffy` a pokusil by se ji zavolat (`iffy(42)`), parser by
přečetl pouze `"if"`, domníval by se, že jde o příkaz `if`, a pak nevěděl co
s `"fy(42)"`, protože očekává otevírací závorku. Naproti tomu `keyword "if"`
se aplikuje pouze na sekvenci znaků `"if"`, za kterou \emph{nenásleduje
písmeno}, čímž zajistíme, že jsme opravdu narazili na klíčové slovo `if`.

#### Konstrukce `split`

Syntaxe pro `split` je přímočará, za klíčovým slovem následují rovnou
složené závorky se seznamem příkazů.

    splitStmt :: Parser Stmt
    splitStmt = do
      keyword "split"
      stmts <- braces $ many stmt
      return $ SplitStmt stmts

### Výrazy

Parsování *výrazů* je o něco složitější, jelikož se musíme vypořádat s
prioritami a asociativitami jednotlivých operátorů.

Gramatiku matematických výrazů můžeme vyjádřit v bezkontextové gramatice takto:

    <expr>     ::= <add-expr>

    <add-expr> ::= <add-expr> "+" <neg-expr>
                 | <add-expr> "-" <neg-expr>
                 | <neg-expr>

    <neg-expr> ::= "-" <mul-expr>
                 | <mul-expr>

    <mul-expr> ::= <mul-expr> "*" <a-expr>
                 | <mul-expr> "/" <a-expr>
                 | <a-expr>

    <a-expr>   ::= variable
                 | literal
                 | "(" <expr> ")"

<figure class="multifigure vertical" id="img-2.3">
  <figcaption>Obrázek 2.3: Příklady parsování výrazu
  <code>2*x/3+(8-y)-z*7</code> </figcaption>

  <figure id="img-2.3-a">
    <div class="image"><img src="../img/parsetree-a.svg" width="400"></div>
    <figcaption>
      (a) Syntaktický strom, reprezentující tento výraz. Binární operace
      (zaznačené kolečky) mají vždy dva operandy, které jsou mohou být číslo,
      proměnná nebo další binární operace.  
    </figcaption>
  </figure>

  <figure id="img-2.3-b">
    <div class="image"><img src="../img/parsetree-b.svg" width="600"></div>
    <figcaption> 
      (b) Ilustrace způsobu, jakým tento výraz zpracuje bezkontextová gramatika.
      Tečkovanými čarami je zaznačena struktura výsledného syntaktického stromu,
      která přímo vyplývá z postupného dělení výrazu na menší části.
    </figcaption>
  </figure>

  <figure id="img-2.3-c">
    <div class="image"><img src="../img/parsetree-c.svg" width="500"></div>
    <figcaption> 
      (c) Tentýž výraz zparsovaný gramatikou PEG se znázorněným výsledným
      syntaktic- kým stromem. Postup parsování již jeho struktuře přímo
      nedpovídá.
    </figcaption>
  </figure>
</figure>

Problém je, že pravidla pro sčítání/odčítání a násobení/dělení jsou rekurzivní
zleva, takže je nelze v této podobě zpracovávat pomocí gramatiky PEG. Proto je
musíme přeformulovat (ošetření mezer jsme pro přehlednost vynechali):

    expr         <- add-expr
    add-expr     <- neg-expr (add-op neg-expr)*
    neg-expr     <- "-"? mul-expr
    mul-expr     <- a-expr (mul-op a-expr)*
    a-expr       <- variable / literal / "(" expr ")" "blaah"

    add-op       <- "+" / "-"
    mul-op       <- "*" / "/"

Tuto PEG gramatiku již můžeme použít, ale struktura gramatiky již neodpovídá
struktuře syntaktického stromu. `parsec` naštěstí obsahuje pomocné
funkce, které nám úkol značně ulehčí. 

Použijeme funkci `chainl1`, jejíž typ je `chainl1 :: Parser a ->
Parser (a -> a -> a) -> Parser a`. `chainl p op` zparsuje jeden a více
výskytů `p` oddělených `op`. Výsledky z `p` postupně odleva
„spojí“ pomocí funkcí vrácených z `op`.

    expr :: Parser Expr
    expr = addExpr <?> "expression"

    addExpr,mulExpr,negExpr,aExpr,varExpr,litExpr :: Parser Expr

    addExpr = chainl1 mulExpr (addOp <* spaces)
    mulExpr = chainl1 negExpr (mulOp <* spaces)

    addOp,mulOp :: Parser (Expr -> Expr -> Expr)
    addOp = 
      BinopExpr AddOp <$ char '+' <|>
      BinopExpr SubOp <$ char '-' 

    mulOp =
      BinopExpr MulOp <$ char '*' <|>
      BinopExpr DivOp <$ char '/' 

    negExpr = (negOp <* spaces) <*> aExpr <|> aExpr

    negOp :: Parser (Expr -> Expr)
    negOp = NegateExpr <$ char '-'

    aExpr = litExpr <|> varExpr <|> parens expr
    varExpr = VariableExpr <$> identifier
    litExpr = LiteralExpr <$> integer

### Pomocné parsery

Nakonec si nadefinujeme drobné parsery, které jsme použili. Každý z nich
zkonzumuje i všechny prázdné znaky, které se za ním nachází, takže se s jejich
ošetřením nemusíme zabývat ve „vyšších“ parserech.

V identifikátorech povolíme i velká písmena a číslice, pokud se nenachází na
začátku.

    integer :: Parser Integer
    integer = read <$> many1 digit <* spaces
    identifier :: Parser String
    identifier = (:) <$> letter <*> many alphaNum <* spaces

    keyword :: String -> Parser ()
    keyword s = string s >> notFollowedBy alphaNum >> spaces

    lparen,rparen,lbrace,rbrace,comma :: Parser ()
    lparen = char '(' >> spaces
    rparen = char ')' >> spaces
    lbrace = char '{' >> spaces
    rbrace = char '}' >> spaces
    comma  = char ',' >> spaces

    parens,braces :: Parser a -> Parser a
    parens = between lparen rparen
    braces = between lbrace rbrace

### PEG gramatika

Na závěr uvedeme kompletní „referenční“ PEG gramatiku Krunimírova jazyka.

    program      <- space* top-stmt* eof
    top-stmt     <- define / stmt

    define       <- "define" space+ identifier 
                    lparen (identifier (comma identifier)*)? rparen
                    lbrace stmt* rbrace

    stmt         <- repeat-stmt / if-stmt / split-stmt / proc-stmt
    repeat-stmt  <- "repeat" lparen expr rparen lbrace stmt* rbrace
    if-stmt      <- "if" space* lparen expr rparen lbrace stmt* rbrace
    split-stmt   <- "split" space* lbrace stmt* rbrace
    proc-stmt    <- "forward" space* lparen expr rparen
                  / "left" space* lparen expr rparen
                  / "right" space* lparen expr rparen
                  / "pen" space* lparen expr rparen
                  / "color" space* lparen expr comma expr comma expr rparen
                  / identifier space* lparen (expr (comma expr)*)? rparen

    expr         <- add-expr
    add-expr     <- mul-expr (add-op space* mul-expr)*
    mul-expr     <- neg-expr (mul-op space* neg-expr)*
    neg-expr     <- (neg-op space*)? a-expr

    add-op       <- "+" / "-"
    mul-op       <- "*" / "/"
    neg-op       <- "-"

    a-expr       <- lit-expr / var-expr / lparen expr rparen
    lit-expr     <- integer
    var-expr     <- identifier

    integer      <- digit+ space*
    identifier   <- letter alpha-num space*

    lparen       <- "(" space*
    rparen       <- ")" space*
    lbrace       <- "{" space*
    rbrace       <- "}" space*
    comma        <- "," space*

    digit        <- [0-9]
    letter       <- [a-zA-Z]
    alpha-num    <- [a-zA-Z0-9]
    space        <- [ \t\r\n\v\f]
    eof          <- !.


## `Krunimir.Trace`

Než představíme vyhodnocování programu reprezentovaného syntaktickým stromem,
musíme ukázat modul `Krunimir.Trace`, který poskytuje datové typy pro práci se
stopami složenými z čar, které za sebou zanechává želva pochodující po písku.
Tyto stopy jsou výstupem funkce `Krunimir.Evaluator.eval`.

    module Krunimir.Trace
    ( Trace(..)
    , Segment(..)
    , prune
    , traceToSegss
    ) where

### Typy

Nejdůležitějším typem je `Trace`, reprezentující stopu želvy. `Trace` má tři
konstruktory:

* `EmptyTrace` je prázdná stopa, tedy nic.
* `SplitTrace` reprezentuje rozdělení stopy na dvě v místě příkazu
  `split`.
* `SegmentTrace` je stopa tvořená úsečkou, za kterou následuje další
  stopa.

    data Trace
      = EmptyTrace
      | SplitTrace Trace Trace
      | SegmentTrace Segment Trace
      deriving Show

Typ `Segment` představuje úsečku mezi dvěma body, která má barvu a tloušťku.

    data Segment = Segment Point Point Color Int 
      deriving Show

    type Point = (Float,Float)
    type Color = (Int,Int,Int)

<figure id="img-2.4">
  <div class="image"><img src="../img/trace.svg" width="600"></div>

  <figcaption>
    Obrázek 2.4: Grafické znázornění stopy. Obrázek nalevo odpovídá vykreslené
    stopě, šipky od jednotlivých segmentů ukazují na příslušné úsečky na
    obrázku.  Důležitý je způsob, jakým se čáry překrývají – segment, jenž je
    blíž kořeni stromu, je překryt segmentem nacházejícím se ve stromu
    „hlouběji“.
  </figcaption>
</figure>

### Funkce

V ostatních modulech budeme potřebovat pomocné funkce `prune` a
`traceToSegss`.

#### Funkce `prune`

Funkce `prune` omezí počet *tahů*, které stopa zahrnuje.

    prune :: Integer -> Trace -> Trace
    prune n img
      | n <= 0 = EmptyTrace
      | otherwise = case img of
        EmptyTrace -> EmptyTrace
        SplitTrace l r -> SplitTrace (prune n l) (prune n r)
        SegmentTrace seg x -> SegmentTrace seg $ prune (n-1) x

#### Funkce `traceToSegss`

Funkce `traceToSegss` transformuje stopu do seznamu seznamů segmentů.
„Vnější“ seznam reprezentuje jednotlivé segmenty jednoho tahu. Tato funkce se
využívá při vykreslování a seřazení jednotlivých segmentů podle tahů zajišťuje
korektní překrytí čar.<sup><a id="fl5" href="#fn5">5</a></sup>

    traceToSegss :: Trace -> [[Segment]]
    traceToSegss EmptyTrace = []
    traceToSegss (SegmentTrace seg img) = [seg]:traceToSegss img
    traceToSegss (SplitTrace limg rimg) = zipSegs (traceToSegss limg) (traceToSegss rimg)
      where
      zipSegs :: [[Segment]] -> [[Segment]] -> [[Segment]]
      zipSegs [] [] = []
      zipSegs lss [] = lss
      zipSegs [] rss = rss
      zipSegs (ls:lss) (rs:rss) = (ls ++ rs):zipSegs lss rss

V případě `SplitTrace` nejprve vyhodnotíme seznamy seznamů segmentů pro
každou stranu zvlášt a pak je pomocnou funkcí `zipSegs` „slijeme“
dohromady.

## `Krunimir.Evaluator`

Nyní se dostáváme k jádru problému, totiž samotnému *vyhodnocování*
Krunimírova programu, implementovanému funkcí `eval`. Vstupem této funkce je
syntaktický strom v podobě typu `Program` (což je jen synonym pro
`[TopStmt]`), výstupem stopa želvy jako typ `Trace`.

    module Krunimir.Evaluator (eval) where
    import Krunimir.Trace
    import Krunimir.Ast

    import qualified Data.Map as M
    import Data.List (genericReplicate)

### Pomocné typy

Definujeme si datový typ `Turtle`, který zahrnuje celý stav želvy -- její
pozici, natočení a barvu a tloušťku pera.<sup><a id="fl6"
href="#fn6">6</a></sup>

    data Turtle = Turtle
      { getPos :: (Float,Float)
      , getAngle :: Integer
      , getColor :: (Int,Int,Int)
      , getPen :: Int
      }

V průběhu vyhodnocování musíme mít uloženy informace o definovaných procedurách
a aktuálních hodnotách argumentů (proměnných). K tomu slouží typ `Env`.
Definujeme si synonyma `ProcMap` a `VarMap`, což jsou *mapy* mapující
jména procedur na jejich definice a jména proměnných na hodnoty.

    data Env = Env ProcMap VarMap
    type ProcMap = M.Map String Define
    type VarMap = M.Map String Integer

### Představení `DiffTrace`

Nyní se musíme rozhodnout, jak přesně budeme vyhodnocování stopy v syntaktickém
stromu implementovat. Určitě bude vhodné vytvořit funkci na vyhodnocení jednoho
příkazu a jednoho výrazu.

Jaké informace potřebujeme k tomu, abychom mohli spočítat hodnotu výrazu?
Vzhledem k tomu, že se ve výrazu můžou vyskytovat argumenty aktuální procedury,
budeme potřebovat `Env`, z něhož získáme hodnoty aktuálních proměnných
(argumentů). Z toho nám plyne typ pro funkci `evalExpr`:

    evalExpr :: Env -> Expr -> Integer

Dále bude třeba vytvořit funkci, která vyhodnotí příkaz. Argumenty této funkce
bude učitě znovu `Env` (potřebujeme znát, jaké procedury existují) a
`Turtle` (musíme vědět, jaký je aktuální stav želvy). 

Jakou hodnotu bychom měli vrátit? Každý příkaz změní stav želvy, proto bychom
novou želvu měli vrátit jako část výsledku. Hlavní je ale to, jestli příkaz
nezmění stopu, kterou za sebou želva zanechá. Jakým způsobem ale tuto změnu
reprezentovat? Nemůžeme použít přímo typ `Trace`, jelikož ten reprezentuje
celou želvinu trasu, kdežto my spočteme jen její začátek, neboť za tímto jedním
příkazem mohou následovat další, které trasu rovněž prodlouží.

Nejlepší bude, když vrátíme *funkci*, která jako argument dostane `Trace`
získaný z následujících příkazů a vrátí novou `Trace`.

Tím získáváme typ:

    evalStmt :: Env -> Stmt -> Turtle -> (Turtle,Trace -> Trace)

S funkcemi typu `Trace -> Trace` budeme pracovat často, proto si vytvoříme
*nový typ*.

    newtype DiffTrace = DiffTrace { applyDT :: Trace -> Trace }

Jaký typ bude mít funkce `applyDT`? Z hodnoty typu `DiffTrace` extrahuje
hodnotu typu `Trace -> Trace`, tudíž její typ bude `DiffTrace -> (Trace ->
Trace)`, neboli `DiffTrace -> Trace -> Trace`. To znamená, že na `applyDT`
můžeme nahlížet jako na funkci se dvěmi argumenty, která *aplikuje*
změnu -- `DiffTrace` -- na `Trace`, čímž získáme novou `Trace`.

Nadefinujeme si také operaci `identity`, tj. žádná změna se neprovede.

    identityDT :: DiffTrace
    identityDT = DiffTrace { applyDT = id }

S tímto novým typem, který reprezentuje *rozdíl* nebo *změnu* stopy
`Trace`, bude typ funkce `evalStmt` vypadat takto:

    evalStmt :: Env -> Stmt -> Turtle -> (Turtle,DiffTrace)

Tuto deklaraci typu můžeme chápat takto: „`evalStmt` je funkce vyžadující
mapu procedur a proměnných uložených v typu `Env`, dále příkaz k vyhodnocení a
stav želvy; vrátí změněný stav želvy a *změnu*, kterou tento příkaz vyvolá
na stopě želvy.“

I když toto typové kung-fu může vypadat na první pohled zbytečně komplikovaně a
složitě, opak je pravdou -- umožní nám vyhodnocování příkazů implementovat velmi
elegantně a jednoduše.

### Funkce `eval`

Nejprve představíme funkce `eval`, která vyhodnotí celý program:

    eval :: Program -> Trace
    eval prog = applyDT (snd $ evalStmts env stmts startTurtle) EmptyTrace
      where

      (defs,stmts) = foldl go ([],[]) (reverse prog)
        where go (ds,ss) topstmt = case topstmt of
                TopDefine def -> (def:ds,ss)
                TopStmt stmt -> (ds,stmt:ss)

      env = Env procMap varMap
      procMap = M.fromList [(defineName def,def) | def <- defs]
      varMap = M.empty

      startTurtle = Turtle
        { getPos = (350.5,350.5)
        , getAngle = 0
        , getColor = (0,0,0)
        , getPen = 0
        }

Nejdříve si rozeberme klauzuli `where`. Nejprve průchodem seznamu `prog`
funkcí `foldl` získáme seznam definic `defs` a seznam příkazů `stmts`,
které extrahujeme z top-příkazů.

Ze seznamu definic vytvoříme mapu procedur `procMap`. Na nejvyšší úrovni se v
programu nenachází žádné proměnné, proto je mapa proměnných prázdná.
`startTurtle` je počáteční stav želvy -- nachází se uprostřed obrázku s
vypnutým černým perem a je otočená směrem nahoru.

V samotném těle funkce `eval` nejprve vyhodnotíme pomocí funkce `evalStmts`
seznam příkazů, čímž získáme dvojici `(Turtle,DiffTrace)`. První prvek, želva,
nás nezajímá, ale funkcí `snd` získáme hodnotu `DiffTrace`, která
reprezentuje změnu, jenž program vykoná na celkové stopě želvy. Tuto změnu
aplikujeme na prázdnou stopu, takže získáme kýženou hodnotu `Trace`.

### Vyhodnocování příkazů

Funkce `evalStmts`, která vyhodnotí seznam příkazů, vždy vyhodnotí jeden
příkaz, poté seznam následujících příkazů a vrátí výslednou želvu a složený
`DiffTrace`.

    evalStmts :: Env -> [Stmt] -> Turtle -> (Turtle,DiffTrace)
    evalStmts _ [] turtle = (turtle,identityDT)
    evalStmts env (stmt:stmts) turtle = 
      let (turtle',dt) = evalStmt env stmt turtle
          (turtle'',dt') = evalStmts env stmts turtle'
      in (turtle'',DiffTrace { applyDT = applyDT dt . applyDT dt' })

V `evalStmt` použijeme velký `case`, v němž patřičně reagujeme na každý druh
příkazu.

    evalStmt :: Env -> Stmt -> Turtle -> (Turtle,DiffTrace)
    evalStmt env stmt = case stmt of
      ForwardStmt e -> forward (ee e)
      LeftStmt e    -> rotate (negate $ ee e)
      RightStmt e   -> rotate (ee e)
      PenStmt e     -> pen (ee e)
      ColorStmt r g b -> color (ee r) (ee g) (ee b)
      RepeatStmt e stmts -> evalStmts env $ concat $ genericReplicate (ee e) stmts
      IfStmt e stmts -> if ee e > 0 then evalStmts env stmts else noop
      SplitStmt stmts -> split $ evalStmts env stmts
      CallStmt name args -> let
          def = lookupDef env name
          binds = zip (defineParams def) (map ee args)
          newenv = makeEnv env binds
        in evalStmts newenv (defineStmts def)
      where ee = evalExpr env

Primitivní operace s želvou jsme ošetřili pomocnými funkcemi, které
implementujeme později. Podmínka a cyklus v podobě `RepeatStmt` a `IfStmt`
jsou implementovány pomocí `evalStmts`, stejně jako volání procedury, kde ale
musíme vytvořit novou mapu proměnných z předaných argumentů.

S výhodou jsme využili *curryingu* -- ač `evalStmt` vyžaduje tři
argumenty, na levé straně rovnice jsme uvedli pouze první dva (`env` a
`stmt`), tudíž na pravé straně musí být funkce typu `Turtle ->
(Turtle,DiffTrace)` (podle typové deklarace funkce `evalStmt`). Tímto se
zbavíme neustálého opakování a předávání argumentu `Turtle` jednotlivým
specializovaným funkcím.  

### Jednotlivé příkazy

Nyní se dostáváme k implementaci jednotlivých funkcí použitých v `evalStmt`.

#### Prázdná operace

Funkci `noop` jsme využili v příkazu `IfStmt` na ošetření situace, když
podmínka neplatí, a její chování je jednoduché -- nedělá nic, takže vrátí
nezměněnou želvu a *identitu*.

    noop :: Turtle -> (Turtle,DiffTrace)
    noop turtle = (turtle,identityDT)

#### Posun vpřed

Při pohybu vpřed musíme želvu posunout na nové místo a zkontrolovat, jestli za
sebou nezanechala čáru. Pokud ano, vrátíme `DiffImage`, který tuto změnu
zachycuje, pokud ne, dostaneme identitu.

    forward :: Integer -> Turtle -> (Turtle,DiffTrace)
    forward len turtle = (turtle',DiffTrace diff) where
      (x,y) = getPos turtle
      ang   = getAngle turtle
      p     = getPen turtle
      x'    = x + sinDeg ang * fromIntegral len
      y'    = y - cosDeg ang * fromIntegral len
      turtle' = turtle { getPos = (x',y') } 
      segment = Segment (x,y) (x',y') (getColor turtle) p
      diff = if p > 0 then SegmentTrace segment else id

Funkce `sinDeg` a `cosDeg`, které počítají sinus a kosinus úhlu ve stupních,
si definujeme později.

#### Otáčení a změny pera

Tyto operace jednoduše změní jednotlivé vlastnosti želvy.

    rotate :: Integer -> Turtle -> (Turtle,DiffTrace)
    rotate ang turtle = (turtle',identityDT) where
      turtle' = turtle { getAngle = getAngle turtle + ang }

    pen :: Integer -> Turtle -> (Turtle,DiffTrace)
    pen p turtle = (turtle',identityDT) where
      turtle' = turtle { getPen = fromIntegral p }

Při změně barvy musíme dbát na to, ať se nějaká ze složek RGB modelu nedostane
mimo povolený rozsah 0 až 255.

    color :: Integer -> Integer -> Integer -> Turtle -> (Turtle,DiffTrace)
    color r g b turtle = (turtle',identityDT) where
      turtle' = turtle { getColor = (crop r,crop g,crop b) }
      crop x
        | x < 0     = 0
        | x > 255   = 255
        | otherwise = fromIntegral x

#### Rozdvojení želvy

Zbývá nám funkce `split`, která implementuje rozdělení želvy. Prvním
parametrem je funkce reprezentující „vedlejší větev“, tedy tělo příkazu
`split { ... }`. Této funkci předáme aktuální želvu, získáme z ní
`DiffTrace`, který následně aplikujeme na `EmptyTrace`, čímž získáme hodnotu
`Trace` reprezentující stopu, kterou „naklonovaná“ želva za sebou
zanechala. Vrátíme stav původní želvy a ve výsledné hodnotě `DiffTrace`
uložíme částečně aplikovaný konstruktor `SplitTrace`.

    split :: (Turtle -> (Turtle,DiffTrace)) -> Turtle -> (Turtle,DiffTrace)
    split f turtle = 
      let (_,dt) = f turtle
          branch = applyDT dt EmptyTrace
      in (turtle,DiffTrace { applyDT = SplitTrace branch })

### Vyhodnocení výrazů

Typ funkce `evalExpr` jsme si představili již dříve, její implementace je
přímočará:

    evalExpr :: Env -> Expr -> Integer
    evalExpr _   (LiteralExpr n) = n
    evalExpr env (VariableExpr name) = lookupVar env name
    evalExpr env (BinopExpr op left right) =
      let a = evalExpr env left
          b = evalExpr env right
      in case op of
        AddOp -> a + b
        SubOp -> a - b
        MulOp -> a * b
        DivOp -> a `div` b
    evalExpr env (NegateExpr expr) = negate $ evalExpr env expr

### Pomocné funkce

Zbývá nám definovat jen pomocné funkce pro vyhledávání proměnných a procedur v
`Env`:

    lookupDef :: Env -> String -> Define
    lookupDef (Env procmap _) name =
      case M.lookup name procmap of
        Just def -> def
        Nothing -> error $ "Undefined procedure " ++ name

    lookupVar :: Env -> String -> Integer
    lookupVar (Env _ varmap) name =
      case M.lookup name varmap of
        Just num -> num
        Nothing -> error $ "Undefined variable " ++ name

Funkce `makeEnv` vytvoří nový `Env` s hodnotami proměnných z
*asociativního seznamu* `binds`:

    makeEnv :: Env -> [(String,Integer)] -> Env
    makeEnv (Env procmap _) binds = Env procmap $ M.fromList binds

A nakonec funkce sinus a kosinus na stupních:

    sinDeg, cosDeg :: Integer -> Float
    sinDeg n = sin $ fromIntegral n * pi / 180.0
    cosDeg n = cos $ fromIntegral n * pi / 180.0

## `Krunimir.PngRenderer`

K renderování stop ve formátu PNG použijeme knihovnu *GD* <a href="bib.html#b4"
class="cite">[4]</a>. Její výhodou je, že je velmi jednoduchá na použití.
Bohužel neumožňuje vykreslovat čáry jiné tloušťky než 1&nbsp;px, takže informaci
o tloušťce pera nemůžeme využít.

    module Krunimir.PngRenderer(renderPng) where
    import Krunimir.Trace
    import qualified Graphics.GD as GD

Veškeré operace s obrázky jsou v knihovně GD implementovány jako operace v
monádě `IO`.

    renderPng :: Trace -> FilePath -> IO ()
    renderPng trace fpath = do
      gimg <- GD.newImage (701,701)
      GD.fillImage (GD.rgb 255 255 255) gimg
      mapM_ (mapM_ $ drawSegment gimg) (traceToSegss trace)
      GD.savePngFile fpath gimg
      where
      drawSegment :: GD.Image -> Segment -> IO ()
      drawSegment gimg (Segment (x1,y1) (x2,y2) (r,g,b) _pen) =
        GD.drawLine (floor x1,floor y1) (floor x2,floor y2) (GD.rgb r g b) gimg

## `Krunimir.SvgRenderer`

Na exportování do SVG nebudeme potřebovat žádnou speciální knihovnu,
jelikož se jedná o formát založený na XML.

    module Krunimir.SvgRenderer(renderSvg) where
    import Krunimir.Trace

Podobně jako v modulu `Krunimir.PngRenderer` si nejprve stopu převedeme funkcí 
`Krunimir.Trace.traceToSegss` na seznam seznamů segmentů. Každému segmentu
poté pouze vytvoříme jeden element úsečky ve tvaru 
<code>&lt;line x1="<i>x1</i>"
y1="<i>y1</i>" 
x2="<i>x2</i>"
y2="<i>y2</i>"
stroke="rgb(<i>červená</i>,<i>zelená</i>,<i>modrá</i>)"
stroke-width="<i>šířka pera</i>"/></code>.

Tyto elementy stačí obalit do kořenového elementu `<svg> ... </svg>`, ve
kterém specifikujeme velikost obrázku, přidat hlavičku a máme hotovo.

    renderSvg :: Trace -> FilePath -> IO ()
    renderSvg trace fpath =
      writeFile fpath $
          svgHeader
        ++ (unlines . map svgSegment . concat . traceToSegss $ trace)
        ++ svgFooter
      where
      svgHeader = "<svg version=\"1.1\" width=\"701\" height=\"701\"\
        \ xmlns=\"http://www.w3.org/2000/svg\">\n"
      svgFooter = "</svg>\n"

      svgSegment (Segment (x1,y1) (x2,y2) (r,g,b) pen) =
        "<line x1=\"" ++ show x1 ++ "\"\
            \ y1=\"" ++ show y1 ++ "\"\
            \ x2=\"" ++ show x2 ++ "\"\
            \ y2=\"" ++ show y2 ++ "\"\
            \ stroke=\"rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++  ")\"\
            \ stroke-width=\"" ++ show pen ++ "\"/>"

## Příklady

Závěrem uvedeme několik rozsáhlejších příkladů, kdy využijeme želví grafiku k
vykreslení několika známých fraktálů. 

### Hilbertova křivka

Hilbertova křivka je plochu vyplňující fraktál popsaný roku 1891 německým
matematikem Davidem Hilbertem. <a href="bib.html#b19" class="cite">[19]</a>
První čtyři iterace této křivky jsou zakresleny na obrázku
[2.5a](#img-2.5-a).

Hlavní část programu je procedura `hilbert(n,side)`, která nakreslí Hilbertovu
křivku `n`-té iterace. Parametr `side` nabývá hodnot `-1` a `1` a
určuje, na kterou stranu se křivka nakreslí. Výsledek programu je na obrázku
[2.5b](#img-2.5-b).

    define hilbert(n,side) {
      left(90*side)
      if(n) {
        hilbert(n-1,-side)
        left(90*side) forward(10)
        hilbert(n-1,side)
        right(90*side) forward(10) right(90*side)
        hilbert(n-1,side)
        forward(10) left(90*side)
        hilbert(n-1,-side)
      }
      left(90*side)
    }

    forward(310) right(90)
    forward(310) right(90)
    pen(1) hilbert(6,-1)

<figure id="img-2.5" class="multifigure vertical">
  <figcaption>Obrázek 2.5: Hilbertova křivka</figcaption>

  <figure id="img-2.5-a">
    <div class="image"><img src="../img/hilbert-gen.svg" width="600"></div>
    <figcaption>(a) Postupné generování Hilbertovy křivky (zleva doprava iterace
    1 až 4).</figcaption>
  </figure>

  <figure id="img-2.5-b">
    <div class="image"><img src="../img/hilbert.svg" width="600"></div>
    <figcaption>(b) Hilbertova křivka vykreslená Krunimírem</figcaption>
  </figure>
</figure>

### Kochova vločka

Kochova vločka je známý fraktál založený na Kochově křivce, kterou v roce 1904
vytvořil švédský matematik Helge von Koch. \cite{wiki:koch-snowflake}

Obrázek [2.6a](#img-2.6-a) zachycuje první čtyři iterace Kochovy křivky. O
kreslení se stará procedura `koch(n)`, jež nakreslí `n`-tou iteraci Kochovy
křivky. Tuto proceduru posléze zavoláme třikrát po sobě, čímž vytvoříme Kochovu
vločku. Výsledek programu je na obrázku [2.6b](#img-2.6-b).

    define koch(n) {
      if(n) {
        koch(n-1)
        left(60) koch(n-1)
        right(120) koch(n-1)
        left(60) koch(n-1)
      }
      if(1-n) {
        forward(2)
      }
    }

    pen(0) forward(-175) left(90) forward(250) right(120)
    pen(1) koch(5) right(120) koch(5) right(120) koch(5)

<figure id="img-2.6" class="multifigure vertical">
  <figcaption>Obrázek 2.6: Kochova vločka</figcaption>

  <figure id="img-2.6-a">
    <div class="img"><img src="../img/koch-gen.svg" width="600"></div>
    <figcaption>(a) Iterace Kochovy křivky. Kochovu vločku dostaneme spojením
    tří Kochových křivek.</figcaption>
  </figure>

  <figure id="img-2.6-b">
    <div class="img"><img src="../img/koch.svg" width="600"></div>
    <figcaption>(b) Kochova vločka vykreslená Krunimírem.</figcaption>
  </figure>
</figure>

### Gosperova křivka

Gosperova křivka, pojmenovaná po svém objeviteli, americkém programátorovi a
matematikovi Billu Gosperovi, je plochu vyplňující fraktál.
<a href="bib.html#b18" class="cite">[18]</a>

Na vykreslení této křivky musíme použít dvojici procedur, `gosperA` a `gosperB`,
přičemž každá kreslí křivku z jiné strany (odpředu a odzadu).  Způsob generování
je zobrazen na obrázku [2.7a](#img-2.7-a), výstup programu na obrázku
[2.7b](#img-2.7-b).

    define gosperA(n) {
      if(n) {
        right(19)  gosperA(n-1)
        left(60)   gosperB(n-1)
        left(120)  gosperB(n-1)
        right(60)  gosperA(n-1)
        right(120) gosperA(n-1) gosperA(n-1)
        right(60)  gosperB(n-1)
        left(79)
      }
      if(1-n) { forward(4) }
    }

    define gosperB(n) {
      if(n) {
        right(79)  gosperA(n-1)
        left(60)   gosperB(n-1) gosperB(n-1)
        left(120)  gosperB(n-1)
        left(60)   gosperA(n-1)
        right(120) gosperA(n-1)
        right(60)  gosperB(n-1)
        left(19)
      }
      if(1-n) { forward(4) }
    }

    left(19) forward(-250) right(30)
    pen(1) gosperA(5)

<figure class="multifigure vertical" id="img-2.7">
  <figcaption>Obrázek 2.7: Gosperova křivka</figcaption>

  <figure id="img-2.7-a">
    <div class="image"><img src="../img/gosper-gen.svg" width="600"></div>
    <figcaption>(a) První tři iterace Gosperovy křivky.</figcaption>
  </figure>

  <figure id="img-2.7-b">
    <div class="image"><img src="../img/gosper.svg" width="600"></div>
    <figcaption>(b) Gosperova křivka vykreslená Krunimírem</figcaption>
  </figure>
</figure>

### Křivka arrowhead

Křivka arrowhead je podobná Sierpińského trojúhelníku, fraktálu polského
matematika Wacłava Sierpińského, který jej popsal v roce 1915.
<a href="bib.html#b17" class="cite">[17]</a>

Podobně jako u Hilbertovy křivky i zde musíme počítat s tím, že křivku musíme
vykreslovat ve dvou zrcadlových variantách, k čemuž využijeme parametr `side`
procedury `arrowhead(n,side)`. Postup generování ukazuje obrázek
[2.8a](#img-2.8-a), výsledek programu obrázek [2.8b](#img-2.8-b).

    define arrowhead(n,side) {
      if(n) {
        left(60*side)
        arrowhead(n-1,-side)
        right(60*side)
        arrowhead(n-1,side)
        right(60*side)
        arrowhead(n-1,-side)
        left(60*side)
      }
      if(1-n) { forward(2) }
    }

    right(90) forward(128) left(60) forward(256) left(120)
    pen(1) arrowhead(8,1)


<figure id="img-2.8" class="multifigure vertical">
  <figcaption>Obrázek 2.8: Křivka arrowhead</figcaption>

  <figure id="img-2.8-a">
    <div class="image"><img src="../img/arrowhead-gen.svg" width="600"></div>
    <figcaption>(a) Prvních pět iterací křivky arrowhead.</figcaption>
  </figure>

  <figure id="img-2.8-b">
    <div class="image"><img src="../img/arrowhead.svg" width="600"></div>
    <figcaption>(b) Křivka arrowhead vykreslená Krunmírem</figcaption>
  </figure>
</figure>

## Závěr

Vytvořili jsme interpret zadaného programovacího jazyka Krunimír, podporující
veškerá rozšíření, vykreslování do dvou grafických formátů, a s velmi
přijatelným výkonem.

Představený program by byl podle zadání ze soutěže nejspíše ohodnocen přibližně
300 body z 333.  Chybějících 33 bodů je za „televizní přenos“, neboli
grafické uživatelské rozhraní (GUI). Tvorba GUI není nijak zvlášť
programátorsky zajímavá, proto ji náš program neimplementuje.

Všechny zdrojové soubory mají dohromady asi 350 řádků kódu. Vezmeme-li v úvahu,
že se jedná o kompletní implementaci netriviálního programovacího jazyka, je
toto číslo poměrně nízké.<sup><a id="fl7" href="#fn7">7</a></sup> Kód je
rozdělen do modulů s minimálními vzájemnými závislostmi, může tedy být snadno
udržován, upravován a rozšiřován.

Pokud bychom namísto Haskellu použili nějaký imperativní programovací jazyk,
například \Cplusplus{}, Ruby  <sup><a id="fl8" href="#fn8">8</a></sup> nebo
dokonce Javu, náš program by byl nejspíš delší a jeho modularita by byla nižší.
Program by pravděpodobně sestával z parseru vytvořeného pomocí nějakého
externího nástroje, který by vytvořil syntaktický strom sestávající z objektů.
Vyhodnocení by bylo sloučeno s vykreslováním a probíhalo by voláním metod
objektů ze syntaktického stromu, jejichž definice by byly roztroušeny u definic
jednotlivých tříd.<sup><a id="fl9" href="#fn9">9</a></sup> Implementovat příkaz
`split` by bylo přinejmenším obtížné.

### Zdrojové kódy

Veškeré soubory související s Krunimírem jsou v repozitáři s prací uloženy ve
složce [`krunimir/`](https://github.com/honzasp/funsp/tree/master/krunimir).
Zdrojové kódy všech uvedených modulů jsou ve složce
[`krunimir/Krunimir`](https://github.com/honzasp/funsp/tree/master/krunimir/Krunimir),
testovací soubory ve složce
[`krunimir/test`](https://github.com/honzasp/funsp/tree/master/krunimir/test).
Část testovacích souborů pochází ze soutěže, část byla vytvořena v rámci této
práce.

Pro testování je možné použít skript
[`runtest.sh`](https://github.com/honzasp/funsp/blob/master/krunimir/runtest.sh),
který spustí program pro všechny soubory ze složky `krunimir/test`. Výsledné
soubory (s příponou `.test.png` a `.test.svg`) je pak možno vizuálně porovnat s
očekávanými výsledky (přípona pouze `.png`).

Složka
[`krunimir/examples`](https://github.com/honzasp/funsp/tree/master/krunimir/examples)
obsahuje zdrojové kódy příkladů, ze kterých se generují obrázky, jež jsou v
práci vloženy.

<div class="footnotes">

  <!-- <sup><a id="fl1" href="#fn1">1</a></sup> -->

  <p id="fn1"><sup><a href="#fl1">1</a></sup> Podobně jako funkce
  <code>main()</code> v jazyku C </p>

  <p id="fn2"><sup><a href="#fl2">2</a></sup> V zadání je specifikováno, že
  nula zadaná jako počet kroků znamená vykreslit celý obrázek, a chování
  našeho programu je odlišné &mdash; nevykreslí nic.</p>

  <p id="fn3"><sup><a href="#fl3">3</a></sup> Zápis pomocí <code>`</code> je
  pouze syntaktický cukr, kterým můžeme zapsat infixově volání jakékoli funkce;
  jinak je ekvivalentí klasickému <code>sepBy p s</code>.</p>

  <p id="fn4"><sup><a href="#fl4">4</a></sup> Parser <code>spaces</code>
  definuje samotná knihovna <code>parsec</code>, má typ <code>Parser ()</code> a
  zahodí nula a více prázdných znaků.</p>

  <p id="fn5"><sup><a href="#fl5">5</a></sup> Kdybychom se rozhodli vzájemné
  překrytí čar zanedbat, celý program by byl <em>výrazně</em> jednodušší.</p>

  <p id="fn6"><sup><a href="#fl6">6</a></sup> Typ <code>Float</code> jsme
  použili namísto obvyklého <code>Double</code> pro ušetření paměti, měl by
  totiž vyžadovat pouze 4 bajty namísto 8. Jelikož Krunimírovy programy
  pracující s desetitisíci želvami nejsou žádnou výjimkou, ušetřené bajty se na
  výkonu programu pozitivně projeví.</p>

  <p id="fn7"><sup><a href="#fl7">7</a></sup> Je nutno podotknout, že program
  nebyl psán tak, aby řádků bylo co nejméně, ale aby byl co nejpřehlednější.</p>

  <p id="fn8"><sup><a href="#fl8">8</a></sup> Pokud bychom využili dynamický
  jazyk jako Ruby, mohli bychom si ušetřit práci a několika jednoduchými
  textovými úpravami převést program pro Krunimíra na program v použitém
  programovacím jazyku, který bychom vyhodnotili pomocí funkce
  <code>eval</code>. Tento postup svého času autor na soutěži úspěšně využil.
  Jedinou nevýhodou je nesnadnost korektní implementace příkazu
  <code>split</code>, jinak jde o řešení téměř dokonalé :-) </p>

  <p id="fn9"><sup><a href="#fl9">9</a></sup> V Javě bychom nejspíše vytvořili
  třídu <code>Statement</code>, která by měla potomky
  <code>ForwardStatement</code>, <code>LeftStatement</code>,
  <code>RepeatStatement</code> apod. přičemž každý by byl ve vlastním souboru...
  </p>

</div>

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

* <code>define <i>procedura</i>(<i>p1</i>,<i>p2</i>,...) { ... }</code> --
  Definuje proceduru procedura, která má libovolný počet parametrů (<i>p1</i>,
  <i>p2</i>, ...). Tyto parametry mohou být v těle procedury použity ve výrazech
  a nabývají hodnoty předané v místě volání.

* <code><i>procedura</i>(arg1,arg2,...)</code> -- Zavolá proceduru *procedura* s
  argumenty (arg1, arg2, ...). Procedura musí být definována *před* svým voláním
  a může být rekurzivní.

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

<figure class="multifigure" id="img-2.2">
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

<div class="footnotes">

  <!-- <sup><a id="fl1" href="#fn1">1</a></sup> -->

  <p id="fn1"><sup><a href="#fl1">1</a></sup> Podobně jako funkce
  <code>main()</code> v jazyku C </p>

  <p id="fn2"><sup><a href="#fl2">2</a></sup> V zadání je specifikováno, že
  nula zadaná jako počet kroků znamená vykreslit celý obrázek, a chování
  našeho programu je odlišné &mdash; nevykreslí nic.</p>

</div>

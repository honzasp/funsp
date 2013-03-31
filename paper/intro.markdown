---
layout: paper
title: Kapitola 1 &mdash; Úvod
---

# {{ page.title }}

Funkcionální programování má původ v λ-kalkulu, matematickém formálním systému,
jež popisuje výpočty jako vyhodnocování výrazů tvořených funkcemi. S funkcemi se
v λ-kalkulu pracuje jako s hodnotami <sup><a id="fl1" href="#fn1">1</a></sup> a
lze je *aplikovat* na argument a získat jejich výsledek nebo je vytvořit pomocí
*λ-abstrakce*. Tento systém je svou výpočetní silou ekvivalentní Turingovu
stroji, je v něm tedy možno vyjádřit jakýkoli strojem proveditelný výpočet.

V *imperativních* programovacích jazycích (C, C++, Java, Ruby, JavaScript,
Befunge, PHP...) jsou algoritmy vyjádřeny jako sekvence kroků – příkazů, které
mění stav programu, zvláště hodnoty proměnných. Postupným vykonáváním těchto
kroků se provádí výpočet. Tento přístup přímo vychází ze strojového kódu
počítačů a v dnešní době jde o dominantní programovací styl.

Ve funkcionálních jazycích se naproti tomu výpočty provádí *vyhodnocováním
výrazů*. Aplikace („zavolání“) funkce má jediný účinek, a to je získání jejího
výsledku. Jelikož neexistuje žádný stav, na němž by výsledek funkce mohl
záviset, je garantováno, že aplikujeme-li funkci vícekrát na stejné argumenty,
dostaneme vždy stejný výsledek. Tato vlastnost se nazývá *referenční
transparentnost*.

Na funkce je tedy možno nahlížet podobně jako na funkce v matematice, takže pro
programátora i pro kompilátor je snadné zjišťovat a dokazovat chování funkcí.
Kompilátor může kód Haskellu snadno optimalizovat <a href="bib.html#b16"
  class="cite">[16]</a>, takže programátor může v programech používat
vysokoúrovňové konstrukce bez obav z negativního dopadu na rychlost programu.  

Další vlastností funkcionálních jazyků je snadná paralelizace. Potřebujeme-li
zjistit hodnotu dvou výrazů, můžeme bez obav každý výpočet provést na
samostatném procesoru, jelikož máme zaručeno, že se výpočty nebudou navzájem
ovlivňovat.

## 1.1 Řešené úlohy

V této práci je představeno řešení dvou úloh z programátorských soutěží s užitím
funkcionálního jazyka Haskell. Úlohy pochází z Ústředních kol Soutěže v
programování, vyhlašované Minister- stvem školství, mládeže a tělovýchovy a
pořádané Národním institutem dětí a mládeže, z let
2010 (Krunimír – želví grafika) a 2012 (Bílá paní – hledání cesty v bludišti).

Tyto úlohy byly vybrány, protože nejsou ani příliš rozsáhlé, ani příliš
jednoduché, a není je možno podezírat, že by byly vytvořeny funkcionálnímu
jazyku „na míru“.

Zadání obou původních úloh je možno nalézt jak na Internetu na stránkách soutěže
(<a href="bib.html#b2" class="cite">[2]</a>, <a href="bib.html#b1"
  class="cite">[1]</a>), tak přetištěné v práci na stranách 61 a 63.

## 1.2 Technická stránka práce

V této práci jsou vytvořeny dva programy, jež řeší tyto úlohy. Kód je zapsán
ve stylu literárního programování, kdy se volně střídá kód srozumitelný pro
počítač a komentář pro člověka, což znamená, že zdrojový kód Haskellu obou
programů je zároveň zdrojovým kódem systému LaTeX, kterým je tato práce
vysázena.

Texty <a href="krunimir.html">kapitol 2</a> a <a href="banshee.html">3</a>
jsou tedy kompletní programy, které je možno přeložit a spustit. Tímto je
zajištěno, že zde prezentovaný kód je plně funkční.

Zdrojový kód práce je spravován pomocí [distribuovaného systému správy verzí
Git](http://git-scm.com/).  Repozitář je volně přístupný na serveru
[github.com](https://github.com), adresa práce je
[https://github.com/honzasp/funsp](https://github.com/honzasp/funsp).

Detailnější popis jednotlivých souborů a složek se nachází v souboru
[README](https://github.com/honzasp/funsp/README).
Jako kompilátor byl využit [GHC](http://www.haskell.org/ghc/) (Glasgow Haskell Compiler), který je dnes mezi překladači
Haskellu dominantní.

## 1.3 Haskell

Haskell je *de-facto* standardní líný čistě funkcionální jazyk. Jeho vývoj začal v září roku 1987,
přičemž první verze jazyka (Haskell 1.0) byla vydána 1. března 1990. Postupně následovaly verze
1.1 až 1.4. V roce 1999 byla vytvořena „stabilní“ verze Haskell 98, která byla později revidována
<a href="bib.html#b11" class="cite">[11]</a>. <a href="bib.html#b9">[9]</a>

Od roku 2006 probíhá proces nazývaný
[Haskell’](http://hackage.haskell.org/trac/haskell-prime/) (Haskell Prime),
jehož cílem je vytvářet nové revize standardu každý rok. První a zatím poslední
revizí je Haskell 2010 <a href="bib.html#b3">[3]</a>.

V následujících podsekcích budou představeny základní vlastnosti jazyka Haskell,
v práci ovšem budeme předpokládat jistou základní znalost funkcionálního
programování v tomto jazyku.  Pokud se čtenář s tímto stylem programování ještě
nesetkal, autor mu může doporučit knihu slovinského studena Mirana Lipovači
[Learn You a Haskell for Great Good](http://learnyouahaskell.com/) <a
  href="bib.html#b14">[14]</a>, která je volně dostupná na Internetu jak v
původní anglické verzi, tak v [částečném českém
překladu](http://naucte-se.haskell.cz/). Práce by nicméně měla být srozumitelná
i se základními znalostmi „klasického“ programování.

### 1.3.1 Čistý funkcionální jazyk

Haskell je *čistý* funkcionální jazyk,<sup><a id="fl5" href="#fn5">5</a></sup>
což znamená, že *všechny* funkce jsou referenčně transparentní a nemají žádný
vnitřní stav, jsou tedy prosty vedlejších účinků.

Většina jazyků, které bývají označováný jako funkcionální, totiž funkce s
vedlejšími účinky povoluje, například pro vykonávání vstupně-výstupních operací
nebo používání měnitelných datových struktur, potřebných pro efektivní
implementaci některých algoritmů. Haskell ovšem pro tyto účely používá jiné
prostředky, především *monády*, které si představíme v podsekci 1.3.5 a které
umožňují zachovat čistotu jazyka.

### 1.3.2 Líné vyhodnocování

Haskell je *líně vyhodnocovaný* jazyk, což znamená, že hodnoty výrazů se
vyhodnocují až ve chvíli, kdy je to nezbytně nutné, přičemž hodnoty, které
nejsou potřeba, se nevyhodnotí vůbec. Tím se nejen zvýší efektivita programu
(neprovádí se zbytečné výpočty), ale hlavně se programátor zbaví nutnosti
zabývat se pořadím vyhodnocování výrazů. Je tedy možné například snadno oddělit
kód produkující a kód konzumující data, čímž se zvýší modularita. <a
  href="bib.html#b10" class="cite">[10]</a>

Ve funkcionálních programech je běžné používat nekonečné struktury, např.
nekonečné seznamy či nekonečně se větvící stromy, a nechat vyhodnotit pouze tu
část dat, která je potřeba k získání požadovaného výsledku.

### 1.3.3 Statické typování

*Statické typování* znamená, že typ každého výrazu je znám již v při kompilaci
programu, takže případné chyby kompilátor odhalí velmi brzy. Není tedy možné,
aby program za běhu zhavaroval na typovou chybu, čímž se eliminuje celá škála
potencionálních bugů. Většinou platí, že když se kód úspěšně zkompiluje, máme
velkou šanci, že bude už napoprvé skutečně fungovat tak, jak si představujeme.

Narozdíl od jazyků jako Java či C++, jež jsou také staticky typované, je mnohem
silnější typový systém Haskellu schopen naprostou většinu typů odvodit sám,
takže typové anotace se obvykle používají jen jako druh dokumentace a způsob
kontroly určené pro člověka (programátora).

### 1.3.4 Typové třídy

S typovým systémem úzce souvisí *typové třídy*. Typové třídy byly do Haskellu
zavedeny, aby se vyřešil problém s „přetěžováním“ funkcí jako je například
porovnávání (`==`), které bychom potřebovali použít s větším množstvím typů
(operace ekvivalence má smysl např. pro čísla, řetězce, seznamy, množiny...).

Ukažme si příklad typové třídy `Eq`, která slouží k implementaci porovnání dvou hodnot:

    class Eq a where
      (==) :: a -> a -> Bool

Tímto deklarujeme, že typ `a` patří do třídy `Eq` právě tehdy, když implementuje
*metodu* `==`.

Mějme dva datové typy:

    data Color = Red | Green | Blue
    data Bit = On | Off

Pro oba tyto typy určitě dává operace porovnání smysl, proto můžeme nadefinovat
*instanci* třídy `Eq`: <sup><a id="fl6" href="#fn6">6</a></sup>

    instance Eq Color where
      Red   == Red   = True
      Green == Green = True
      Blue  == Blue  = True
      _     == _     = False

    instance Eq Bit where
    On  == On  = True
    Off == Off = True
    _   == _   = False

Touto deklarací specifikujeme, že typy `Color` a `Bit` jsou instancemi třídy
`Eq`, a poskytneme implementaci metody `==`. Nyní můžeme používat funkci `==` s
barvami i bity, např. `Red == Red` vrátí `True` a `On == Off` vrátí `False`.
Zároveň ale nemůžeme omylem porovnávat barvy a bity – napíšeme-li `Red == On`,
kompilátor ohlásí typovou chybu, jelikož funkce `==` akceptuje pouze hodnoty
stejného typu.

Kdybychom chtěli funkci, která otestuje, jestli se daný prvek nachází v seznamu, mohli
bychom ji nadefinovat takto:

    elem :: Eq a => a -> [a] -> Bool
    elem _ [] = False
    elem x (y:ys) = if x == y then True else elem x ys

Typ této funkce, `Eq a => a -> [a] -> Bool`, odráží skutečnost, že tato funkce
je definovaná pouze pro takové typy `a`, které náleží do třídy `Eq`. Můžeme ji
tedy použít jak na seznam barev (`[Color]`), tak na seznam bitů (`[Bit]`).

Instancemi tříd samozřejmě nemusí být jen takovéto jednoduché typy. Můžeme si
nadefinovat typ reprezentující binární strom:

    data Tree a = Node (Tree a) (Tree a) | Leaf a

Tento typ bychom obratem mohli učinit instancí třídy `Eq`:

    instance Eq a => Eq (Tree a) where
      Leaf x == Leaf y         = x == y
      Node l1 r1 == Node l2 r2 = l1 == l2 && r1 == r2
      _ == _ = False

Tato instance deklaruje, že pro všechny typy `a` náleží typ `Tree a` do třídy
`Eq`, platí-li, že typ `a` náleží do třídy `Eq`.

### 1.3.5 Monadický vstup/výstup

Jak už bylo zmíněno výše, vyhodnocování výrazů v čistě funkcionálním jazyce
nemůže mít žádné vedlejší efekty a musí být referenčně transparentní. Co když
ale potřebujeme vykonat nějakou vstupně/výstupní operaci, např. přečíst znak,
který uživatel napsal na klávesnici, nebo zapsat soubor na disk? Takové „funkce“
by určitě vedlejší efekt měly a referenčně transparentní jistě také nejsou.

Většina jiných funkcionálních jazyků tento problém řeší tak, že obětuje
*čistotu* a takové „nefunkcionální funkce“ povoluje, přičemž starosti s
porušením referenční transparentnosti nechává na programátorovi. Tento přístup
ovšem není možný v líně vyhodnocovaném jazyku, jelikož nemáme žádnou kontrolu
nad tím, v jakém pořadí a jestli vůbec se funkce „zavolají“.

#### Typ IO

Haskell proto používá jinou techniku, která umožňuje zachovat funkcionální
čistotu i líné vyhodnocování. Standardní knihovna poskytuje typ `IO a`, který
reprezentuje vstupně/výstupní operaci, jejíž výsledek je typu `a`.

Ukažme si příklad několika operací ze standardní knihovny:

    getChar :: IO Char
    getLine :: IO String
    putStrLn :: String -> IO ()
    readFile :: FilePath -> IO String
    writeFile :: FilePath -> String -> IO ()

`getChar` je vstupně/výstupní operace, která přečte jeden znak zadaný uživatelem
a jejím výsledkem je tento znak, tedy typ `Char`. Obdobně `getLine` přečte celý
řádek a její výsledek je typu `String`. Chceme-li naopak řádek vypsat, můžeme
použít `putStrLn`, což je funkce, které předáme řetězec, jež si přejeme vypsat,
a dostaneme vstupně/výstupní operaci, která tento řetězec vypíše. Jelikož tato
operace nemá žádný smyslupný výsledek, vrací tzv.  *nulový typ* `()`, který má
jedinou hodnotu (rovněž se zapisuje ()) a používá se jako „výplň“. 

Abychom přečetli soubor, musíme znát jeho umístění, proto je `readFile` funkce,
které předáme cestu k souboru<sup><a id="fl8" href="#fn8">8</a></sup> a
dostaneme vstupně/výstupní operaci, jejíž výsledek bude obsah souboru jako
řetězec (`String`). Chceme-li zapsat data do souboru, použijeme funkci
`writeFile`, které předáme dva argumenty – cestu k souboru a řetězec znaků,
které si přejeme zapsat – a dostaneme IO operaci. Výsledek této operace je opět
`()`.

Existuje pouze jedna možnost, jak vykonat operaci reprezentovanou typem `IO` –
definovat ji jako proměnnou `main` v modulu `Main`.<sup><a id="fl9"
    href="#fn9">9</a></sup> Tato hodnota má typ `IO α`. Spustit program v
Haskellu tedy vlastně znamená vyhodnotit operaci, která je přiřazena do `main`,
a výsledek typu `α` zahodit.

Pokud bychom tedy po programu chtěli, ať spočte jednoduchý příklad a výsledek
vypíše, mohli bychom nadefinovat `main` takto (funkce `++` slouží ke spojení
dvou řetězců a funkce `show` převede číslo na řetězec):

    main = putStrLn ("Jedna plus jedna je " ++ show (1+1))

#### Spojování operací pomocí `>>=` a `>>`

Co kdybychom ale chtěli provést více operací, například se zeptat uživatele na
jméno a pak ho pozdravit? Potřebujeme nějakým způsobem „slepit“ dvě IO operace,
a to takovým způsobem, aby druhá mohla využít výsledek první. K tomu slouží
funkce `>>=`, někdy též nazývaná „bind“.  Její typ je takovýto:

    (>>=) :: IO a -> (a -> IO b) -> IO b

Jako první argument jí předáme první vstupně/výstupní operaci, kterou si přejeme
vykonat a její výsledek dát jako vstupní hodnotu funkci předané jako druhý
argument. Výsledek této funkce „zabalený“ v typu `IO` je výsledkem celé `>>=`.

S její pomocí můžeme uživatele pozdravit takto: <sup><a id="fl10"
    href="#fn10">10</a></sup>

    main = getStrLn >>= (\name -> putStrLn ("Ahoj " ++ name))

Jak se ale uživatel doví, že po něm chceme, aby napsal svoje jméno? Nejprve mu
musíme sdělit, že po něm požadujeme zadat jméno, a až poté jej přečíst a
pozdravit. Opět můžeme využít `putStrLn` a `>>=`:

    main = 
      putStrLn "Kdo tam?" >>= (\_ ->
        getStrLn >>= (\name ->
          putStrLn ("Ahoj " ++ name)))

Výsledek z prvního `putStrLn` nás nazajímá (koneckonců to je pouze nulový typ
`()`), takže jsme jej přiřadili do speciální „proměnné“ `_`, která funguje jako
„černá díra“ – můžeme do ní přiřazovat nepotřebné údaje. Tento způsob zacházení
s výsledky vstupně/výstupních operací je poměrně častý, proto je definována
funkce `>>` s typem `IO a -> IO b -> IO b`, která je podobná `>>=`, s tím
rozdílem, že první operaci sice provede, ale její výsledek zahodí a následně
provede druhou operaci (nepředáváme jí tedy funkci), jejíž výsledek vrátí.

    main =
      putStrLn "Kdo tam?" >>
        getStrLn >>= (\name ->
          putStrLn ("Ahoj " ++ name))

Tento kód ale není příliš přehledný a pokud bychom jej chtěli ještě dále
rozšířit, mohli bychom se v něm brzy ztratit. Protože zřetězené používání `>>=`
a `>>` je v Haskellu velmi časté, obsahuje jazyk tzv. `do`-syntaxi, která nám
umožňuje zapisovat série takovýchto operací o něco úhledněji:

    main = do
      putStrLn "Kdo tam?"
      name <- getStrLn
      putStrLn ("Ahoj " ++ name)

#### Monády

Typ `IO` je jen jedním z příkladů *monád*. Monády, původně koncept z teorie
kategorií, je v Haskellu možno považovat za *výpočty* nebo *akce*, které je
možno *skládat*, tedy přesměrovat *výstup* z jedné monády do druhé.

Do Haskellu jsou monády zahrnuty pomocí typové třídy *Monad* ze standardní
knihovny jazyka.  Tato třída je definována takto:

    class Monad m where
      (>>=)  :: m a -> (a -> m b) -> m b
      (>>)   :: m a -> m b -> m b
      m >> k  = m >>= \_ -> k

      return :: a -> m a


Funkce `>>=` (nazývaná „bind“) a `>>`, které jsme si výše ukázali při použití s
typem `IO`, jsou tedy ve skutečnosti definované pro všechny monády. Funkce
`return` pak pro libovolnou hodnotu vytvoří monádu, jejímž výstupem je tato
hodnota.

Kromě typu `IO` jsou ze standardní knihovny monádami například i typy `[]`
(seznam), `Maybe`, `Either e` či `(->) a` (funkce, jejímž argumentem je typ a).
V podsekci <a href="banshee.html#subsec-st">3.6.4</a>  si představíme monádu `ST
s`, kterou je možno považovat za „odlehčený“ typ `IO`, umožňující v čistém kódu
využívat měnitelné datové struktury a efektivně implementovat algoritmy, které
takovéto struktury vyžadují.

## 1.4 Dokumentace

Veškeré knihovny použité v této práci se, včetně dokumentace, nachází na serveru
[Hackage](http://hackage.haskell.org/packages/hackage.html). Pro vyhledávání v
dokumentaci je možno použít vyhledávač [Hoogle](http://www.haskell.org/hoogle/).
Tento vyhledávač umožňuje vyhledávat funkce, datové typy či třídy podle jména,
ale dokáže najít funkci i podle přibližného *typu*, což z něj činí velmi
užitečný nástroj.

<div class="footnotes">

  <!-- <sup><a id="fl1" href="#fn1">1</a></sup> -->

  <p id="fn1"><sup><a href="#fl1">1</a></sup> Základní λ-kalkulus dokonce žádné
  jiné hodnoty než funkce nezná.</p>

  <p id="fn5"><sup><a href="#fl5">5</a></sup> Někdy se používá
  rovněž označení „čistě funkcionální jazyk“, které ovšem není úplně přesné.</p>

  <p id="fn6"><sup><a href="#fl6">6</a></sup> Haskell umožňuje pro datové typy
  automaticky odvodit instance tříd ze standardní knihovny, jako je
  <code>Eq</code> nebo <code>Ord</code>, pomocí klauzule <code>deriving</code>,
  takže jen zřídka definujeme operaci <code>==</code> takto „ručně“.</p>

  <p id="fn7"><sup><a href="#fl7">7</a></sup> Jde o jistou obdobu typu
  <code>void</code> z jazyka C nebo <code>nil</code> či <code>null</code> z
  dynamických jazyků jako Ruby či JavaScript, která je ovšem typově bezpečná.
  </p>

  <p id="fn8"><sup><a href="#fl8">8</a></sup>Typ <code>FilePath</code> je
  synonym k typu <code>String</code>, který se ve standardní knihovně používá
  pro označení cest k souboru.</p>

  <p id="fn9"><sup><a href="#fl9">9</a></sup>„Proměnná“ samozřejmě neznamená, že
  se její hodnota nějakým způsobem mění; tento pojem se používá podobně jako v
  matematice. V imperativním jazyku bychom řekli „konstanta“.  </p>

  <p id="fn10"><sup><a href="#fl10">10</a></sup>Syntaxe <code>\x -> E</code>
  znamená anonymní (lambda) funkci s parametrem <code>x</code> a tělem
  <code>E</code>.</p>

</div>

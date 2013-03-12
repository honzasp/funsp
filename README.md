Líně, čistě, funkcionálně
=========================

Toto je repozitář práce SOČ oboru 18 (Informatika) nazvané "Líně, čistě,
funkcionálně".

Abstrakt
--------

Funkcionální programování, vycházející z λ-kalkulu, tvoří alternativu k dnes
široce využívanému imperativnímu přístupu k programování, reprezentovanému
jazyky jako C, C++, Java, C#, Objective-C, JavaScript, Python, Ruby či PHP.

V této práci je představeno řešení dvou úloh Ústředního kola ČR Soutěže v
programování z let 2010 a 2012 s využitím líně vyhodnocovaného funkcionálního
jazyka Haskell. První úloha je implementace jednoduchého programovacího
jazyka, podobného jazyku Logo, sloužícího k vykreslování želví grafiky. Druhá
úloha se zabývá hledáním nejkratší cesty v bludišti s pohyblivými překážkami
a omezenou schopností procházet zdmi.

Cílem práce je prezentovat funkcionální programování a techniky, které se
v něm využívají, a ukázat, že tento programovací styl není pouze předmětem
akademického výzkumu, ale že se dá využít i k řešení „skutečných“ úloh.


Sazba práce
-----------

Práce se generuje pomocí systému LaTeX z následujících souborů:

* `paper.tex`
* `tex/*.tex`
* `krunimir/Krunimir/*.lhs`
* `banshee/Banshee/*.lhs`

K vysázení je možno využít program `make`:

* `make` nebo `make paper.pdf` pomocí programu pdfLaTeX vytvoří soubor
  PDF `paper.pdf` s vysázenou prací
* `make watch` sleduje změny provedené ve zdrojových souborech a automaticky
  překompiluje soubor `paper.pdf`

Přeložení programů Krunimir a Banshee
-------------------------------------

K přeložení programů je nutno mít nainstalovaný překladač GHC s instalátorem
balíčků Cabal. Poté stačí v příslušné složce spustit:

    cabal configure
    cabal build

Výsledné spustitelné soubory se nachází ve složce `dist/build`.

Nepůvodní soubory
----------------

Obsah následujících složek je převzat z doprovodných souborů k Soutěži v
programování, které jsou volně dostupné na [webu soutěže](http://stv.cz/sp/).

* `banshee/test/data_testovaci`
* `banshee/test/data_ukazkova`
* `krunimir/test/zaklad`
* `krunimir/test/rozsireni2`
* `krunimir/test/rozsireni3`
* `krunimir/test/rozsireni4`
* `krunimir/test/vsechno`

Ze stejného zdroje pochází soubory se soutěžním zadáním:

* `banshee/mcr12zp.pdf`
* `krunimir/mcr2010z.pdf`

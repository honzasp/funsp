Líně, čistě, funkcionálně
=========================

Toto je repozitář práce SOČ oboru 18 (Informatika) nazvané "Líně, čistě,
funkcionálně".

Abstrakt
--------

Funkcionální programování, vycházející z λ-kalkulu, tvoří alternativu k dnes
široce využívanému imperativnímu přístupu k programování, reprezentovanému
jazyky jako C, C++, Java, C#, Objective-C, JavaScript, Python, Ruby či PHP.

Tyto jazyky jsou založeny na postupném vykonávání příkazů, jež mění stav
programu (zvláště hodnoty proměnných). Naproti tomu funkcionální jazyky provádí
výpočty vyhodnocováním výrazů. Základní stavební prvky – funkce – jsou od
imperativních protějšků (ať už procedur, metod nebo „funkcí“ jako v jazyce C)
odlišné tím, že jejich výsledek vždy závisí pouze na vstupních argumentech, čímž
se přibližují matematickým funkcím.

Algoritmy zapsané ve funkcionálním jazyce tudíž nemají podobu série kroků, jež
se musí postupně vykonat a jež určují jak se k výsledku dostat, ale spíše
popisují co je jejich výsledkem.

V této práci je představeno řešení dvou úloh Ústředního kola ČR Soutěže v
programování z let 2010 a 2012 užitím funkcionálního programovacího jazyka
Haskell. Cílem je čtenáře seznámit s vysokoúrovňovými koncepty, které se ve
funkcionálních programech používají, a jejich aplikací při řešení „skutečných“
úloh.

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

# 2. projekt do predmetu FLP
## Autor: Martin Zmitko
## Login: xzmitk01
## Datum: 2024-04-28
## Zadani: Hamiltonovska kruznice

# Popis reseni
O hledani hamiltonovskych kruznic v grafu (nacteneho ze stdin pomoci funkci z input2.pl
a ulozeneho ve forme dynamickeho predikatu `edge/2`) se stara predikat `hamilton_circle`.
Algoritmus si udrzuje momentalni uzel a cestu (ve forme seznamu uzlu), do ktere pridava prvni dalsi
nalezeny uzel, jeste v ni neobsazeny. Pro optimalizaci prohledavani jsou zahozeny jiz nalezene cesty
v dynamickem predikatu `path/1`. Pri nacteni grafu se zjisti celkovy pocet uzlu ktery se ulozi jako
dynamicky predikat `nodeCount/1`. Cesta je hamiltonovskou kruznici prave, kdyz jeji delka se rovna
poctu uzlu.

K nalezeni vsech dostupnych cest je vyuzit predikat findall, ktery pro vsechny uzly v grafu
vytvori vsechny mozne hamiltonovske cesty. Protoze jsou cesty zatim ulozeny jako seznamy
uzlu, je potreba je prevest na seznamy hran ([a,b,c] -> [a-b,b-c,c-a]), coz zajistuje `nodes_to_edges`,
finalni propojeni do kruznice pak `connect_path`.

Protoze algoritmus nalezne vsechny mozne kruznice a jejich seznam tedy obsahuje jeji vsechny
mozne permutace poradi pro vsechny uzly, pro nalezeni unikatnich cest jsou nejdrive porovnany
a pripadne prohozeny uzly v hranach a cesty nasledne serazeny (vse podle abecedy).
Nasledna aplikace predikatu sort pote zajisti unikatnost.


# Pouziti
Program se prelozi pomoci prilozeneho Makefile prikazem `make`, vytvori se spustitelny soubor `flp23-log`.

Program cte vstupni graf ze standardniho vstupu a vypisuje na standardni vystup.

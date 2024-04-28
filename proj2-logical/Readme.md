# 2. projekt do predmetu FLP
## Autor: Martin Zmitko
## Login: xzmitk01
## Datum: 2024-04-28
## Zadani: Hamiltonovska kruznice

# Popis reseni
O hledani hamiltonovskych kruznic v grafu (nacteneho ze stdin pomoci funkci z input2.pl
a ulozeneho ve forme dynamickeho predikatu edge/2) se stara predikat hamilton_circle.
Tento predikat rika, ze cesta v grafu je hamiltonovou kruznici, k tomu vyuziva pocet Count
ktery pri dalsim zpracovanem uzlu dekrementuje, uspech je pouze, kdyz ma cesta spravnou delku.
Algoritmus si udrzuje momentalni uzel a cestu (ve forme seznamu uzlu), do ktere pridava prvni
nalezeny uzel, jeste v ni neobsazeny. Pro optimalizaci prohledavani jsou zahozeny jiz nalezene cesty
v dynamickem predikatu path/1.

K nalezeni vsech dostupnych cest je vyuzit predikat findall, ktery pro vsechny uzly v grafu
vytvori vsechny mozne hamiltonovske cesty. Protoze jsou cesty zatim ulozeny jako seznamy
uzlu, je potreba je prevest na seznamy hran ([a,b,c] -> [a-b,b-c,c-a]), coz zajistuje nodes_to_edges,
finalni propojeni do kruznice pak connect_path.

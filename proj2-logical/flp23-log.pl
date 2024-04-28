/**
 * Projekt 2 (logicky) z FLP
 * Autor: Martin Zmitko (xzmitk01@stud.fit.vutbr.cz)
 * Datum: 2021-04-28
 * Popis: Program nalezne vsechny hamiltonovy kruznice v grafu
 */

:- dynamic edge/2. % pro hrany
:- dynamic path/1. % pro optimalizaci hledani cest
:- dynamic circle/1. % pro nalezeni unikatnich kruznic

/** Input/Output, implementace z input2.pl */

/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).


/** Implementace z input2.pl konci zde */


/** vytvori hrany grafu jako dynamicke predikaty ze seznamu hran*/
make_edges([]).
make_edges([H|T]) :-
	[[N1], [N2]] = H,
	assertz(edge(N1, N2)),
	assertz(edge(N2, N1)), % obousmerna hrana
	make_edges(T).


/** vypise kruznici v pozadovanem formatu*/
write_circle([H]) :-
	writeln(H).
write_circle([H|T]) :-
	write(H), write(' '),
	write_circle(T).


/** Nalezena cesta je hamiltonova kruznice */
hamilton_circle(1, Node, [Next|RestPath], [Next|RestPath]) :-
	\+ path([Next|RestPath]), % pokud cesta neexistuje
	edge(Node, Next), % existuje hrana mezi pocatecnim a koncovym uzlem
	assertz(path([Next|RestPath])).

/** Predikat pro nalezeni hamiltonovy kruznice
 *  Count - pocet uzlu, ktere zbyvaji pridat do cesty
 *  Node - aktualni uzel
 *  Path - dosavadni cesta
 *  Result - vysledna kruznice jako seznam uzlu
 */
hamilton_circle(Count, Node, Path, Result) :-
	edge(Node, NewNode), % existuje hrana mezi aktualnim a dalsim uzlem
	\+ member(NewNode, Path), % dalsi uzel neni v ceste
	append(Path, [NewNode], NewPath), % pridani dalsiho uzlu do cesty
	NewCount is Count - 1, % snizeni poctu uzlu, ktere zbyvaji pridat do cesty
	hamilton_circle(NewCount, NewNode, NewPath, Result).


/** Prevod seznamu seznamu cest jako uzlu na seznam seznamu hran 
 * [[a,b,c],[b,c,a]] -> [[a-b,b-c],[b-c,c-a]]
*/
nodes_to_edges([], []).
nodes_to_edges([Nodes|NodesRest], [Edges|EdgesRest]) :-
	nodes_to_edges_(Nodes, Edges),
	nodes_to_edges(NodesRest, EdgesRest).

/** Prevod seznamu uzlu na seznam hran bez posledni */
nodes_to_edges_([], []).
nodes_to_edges_([_], []).
nodes_to_edges_([Node1,Node2|Rest], [Node1-Node2|Edges]) :-
    nodes_to_edges_([Node2|Rest], Edges).


/** Spojeni prvniho a posledniho uzlu v seznamu kruznic 
 * [[a-b,b-c],[b-c,c-a]] -> [[a-b,b-c,c-a],[b-c,c-a,a-b]]
*/
connect_paths([], []).
connect_paths([Path|Rest], [Result|ResultList]) :-
	connect_path(Path, Result),
	connect_paths(Rest, ResultList).

/** Spojeni prvniho a posledniho uzlu v kruznici */
connect_path(Unclosed, Closed) :-
	[First-_|Tail] = Unclosed,
	last(Tail, _-Last),
	[First-Last|Unclosed] = Closed.


/** Predikat pro prohozeni paru */
swap(X-Y, Y-X) :- X @> Y.
swap(X-Y, X-Y) :- X @< Y.

/** Pruchod kruznici a pripadne prohozeni paru v hranach */
sort_pairs([], []).
sort_pairs([Pair|Pairs], [SortedPair|SortedPairs]) :-
	swap(Pair, SortedPair),
	sort_pairs(Pairs, SortedPairs).

/** Serazeni vyslednych kruznic pro zajisteni unikatnosti */
sort_circles([], []).
sort_circles([Circle|Rest], [SortedCircle|SortedRest]) :-
	sort_pairs(Circle, SortedPairs), % Prohodit pary v hranach
	sort(SortedPairs, SortedCircle), % Seradit hrany
	sort_circles(Rest, SortedRest).


main :-
	prompt(_, ''),
	read_lines(LL),
	split_lines(LL,S),
	make_edges(S),
	findall(Nodee, edge(Nodee, _), DupNodes), % ziska seznam vsech uzlu
	sort(DupNodes, Nodes), % zajisteni unikatnosti uzlu
	length(Nodes, N), % zjisti pocet uzlu == delka vysledne kruznice

	% najde vsechny hamiltonovy kruznice pro vsechny pocatecni uzly jako seznamy uzlu do Paths
	findall(Path, (edge(Node, _), hamilton_circle(N, Node, [Node], Path)), Paths),
	nodes_to_edges(Paths, Circles),
	connect_paths(Circles, ConnectedCircles),

	% serazeni kruznic pro zajisteni unikatnosti
	sort_circles(ConnectedCircles, SortedFirst),
	sort(SortedFirst, UniqueCircles),
	maplist(write_circle, UniqueCircles),
	halt.

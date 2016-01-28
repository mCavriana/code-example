/* Cavriana Massimo
	Ultima modifica:04/12/2013

*/

:- consult(heap).
:- consult(graph).

:- dynamic dist/3.		% dist(G, V, D).
:- dynamic visited/2.		% visited(G, V).
:- dynamic previous/3.		% previous(G, V, U).
:- dynamic passed/1.		% passed(arc).

% Inizializza le distanze dei vertici a "infinito"
initialize(_, []) :- !.
initialize(G, [V|Xs]) :- assert(dist(G, V, 2e+60)), initialize(G, Xs).

% Sostituisce le precedenti distanze con NewDist
change_dist(G, V, NewDist) :- retract(dist(G, V, _)),
							assert(dist(G, V, NewDist)), !.

% Sostituisce i vecchi precedenti con U
% Se non esiste un predicato previous associato lo crea semplicemente
change_previous(G, V, U) :- not(previous(G, V, _)),
	assert(previous(G, V, U)), !.
change_previous(G, V, U) :- retract(previous(G, V, _)),
	assert(previous(G, V, U)), !.

% Caso base: lo heap e' vuoto
% Passo Ricorsivo:
% estrae il vertice con distanza minore e lo visita
search_smallest(_, Q) :- empty(Q), !.
search_smallest(G, Q) :- extract(Q, _, U), assert(visited(G, U)),
	neighbors(G, U, Ns), search_nb(G, Q, U, Ns),
	search_smallest(G, Q).

% Aggiorna le distanze dei vicini da U.
% Caso base: ho visitato tutti i vicini.
% Trovo un vicino con distanza maggiore, non aggiorno la distanza.
% Trovo un vicino con distanza minore già  visitato, aggiorno la
% distanza ma non lo inserisco nello heap.
% Trovo un vicino con distanza minore non ancora visitato, aggiorno
% la distanza e lo inserisco nello heap.
% Trovo un vicino con distanza minore, aggiorno la distanza,
% V e' nello heap, quindi aggiorno la sua chiave.
search_nb(_, _, _, []) :- !.
search_nb(G, Q, U, [N|Xs]) :- dist(G, U, D), arg(4, N, Weight),
	NewCalcDist is (D+Weight), arg(3, N, V),
	dist(G, V, D2),
	NewCalcDist >= D2,
	search_nb(G, Q, U, Xs), !.
search_nb(G, Q, U, [N|Xs]) :- dist(G, U, D), arg(4, N, Weight),
	NewCalcDist is (D+Weight), arg(3, N, V),
	dist(G, V, D2), visited(G, V),
	NewCalcDist < D2, change_dist(G, V, NewCalcDist),
	change_previous(G, V, U),
	search_nb(G, Q, U, Xs), !.
search_nb(G, Q, U, [N|Xs]) :- dist(G, U, D), arg(4, N, Weight),
	NewCalcDist is (D+Weight), arg(3, N, V),
	dist(G, V, D2), not(heap_entry(Q, _, _, V)),
	NewCalcDist < D2, change_dist(G, V, NewCalcDist),
	change_previous(G, V, U), not(visited(G, V)),
	insert(Q, NewCalcDist, V),
	search_nb(G, Q, U, Xs), !.
search_nb(G, Q, U, [N|Xs]) :- dist(G, U, D), arg(4, N, Weight),
	NewCalcDist is (D+Weight), arg(3, N, V),
	dist(G, V, D2),
	NewCalcDist < D2, change_dist(G, V, NewCalcDist),
	change_previous(G, V, U),
	modify_key(Q, NewCalcDist, _, V),
	search_nb(G, Q, U, Xs), !.


% Scrive nella base di dati tutti i percorsi piu'
% brevi dalla sorgente "Source" ad ogni vertice
sssp(G, Source) :- vertices(G, Vs), initialize(G, Vs),
	change_dist(G, Source, 0),
	new_heap(q), insert(q, 0, Source),
	search_smallest(G, q).

% Path e' la lista contenente gli archi indicanti il cammino minimo
% dalla Source a V
shortest_path(_, Source, Source, Path) :-
	findall(X, passed(X), Path),
	retractall(passed(_)), !.
shortest_path(G, Source, V, Path) :- vertex(G, V), vertex(G, Source),
	previous(G, V, U),
	arc(G, U, V, W),
	asserta(passed(arc(G, U, V, W))),
	shortest_path(G, Source, U, Path), !.




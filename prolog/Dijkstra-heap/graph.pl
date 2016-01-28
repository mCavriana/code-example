/* Cavriana Massimo
	Ultima modifica: 01/12/2013

*/


:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

% Aggiunge un nuovo grafo G se non esiste gia'  e se G e' un atomo
new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)).

% Aggiunge il nodo V al grafo G
add_vertex(G, V) :- vertex(G, V), !.
add_vertex(G, V) :- graph(G), assert(vertex(G, V)).

% Aggiunge un arco tra U e V con peso Weight
% solo se non esiste gia'  un arco tra U e V.
add_arc(G, U, V) :- add_arc(G, U, V, 1).
add_arc(G, U, V, _) :- arc(G, U, V, _), !.
add_arc(G, U, V, Weight) :- graph(G), vertex(G, U), vertex(G, V), Weight>0,
							assert(arc(G, U, V, Weight)), !.

% Cancella il grafo G e tutti i suoi archi e vertici.
delete_graph(G) :- graph(G), retractall(arc(G, _, _, _)),
					retractall(vertex(G, _)), retract(graph(G)).

% Vs e' una lista contenente tutti i vertici di G.
vertices(G, Vs) :- findall(X, vertex(G, X), Vs).

% Es e' una lista di tutti gli archi presenti in G.
arcs(G, Es) :- findall(arc(G, V, U, Weight), arc(G, V, U, Weight), Es).

% Ns e' una vista contenente gli archi uscenti da V.
neighbors(G, V, Ns) :- findall(arc(G, V, N, Length), arc(G, V, N, Length), Ns).

% Stampa una lista dei vertici del grafo G
list_vertices(G) :- listing(vertex(G, _)).

% Stampa una lista degli archi del grafo G.
list_arcs(G) :- listing(arc(G, _, _, _)).

% Stampa una lista dei vertici e degli archi del grafo G.
list_graph(G) :- list_vertices(G), list_arcs(G).

/* Cavriana Massimo
	Ultima modifica: 04/12/2013

*/
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic last_extracted/1.

% Crea un nuovo albero.
new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.

% Cancella l'albero e tutti i nodi.
delete_heap(H) :- heap(H, _), retract(heap(H, _)),
	retractall(heap_entry(H, _, _, _)).

% S è la dimensione corrente dello heap H.
heap_size(H, S) :- heap(H, S).

% H è vuoto?
empty(H) :- heap(H, 0).
not_empty(H) :- not(empty(H)).

% Restituisce la testa di H.
head(H, K, V) :- heap_entry(H, 1, K, V).

% Inserisce chiave e valore, purchè il valore non esista gia' .
insert(H, K, V) :- heap(H, 0), last_extracted(_),
	retractall(last_extracted(_)), insert(H, K, V), !.
insert(H, K, V) :- heap(H, 0), retract(heap(H, 0)),
	assert(heap(H, 1)), assert(heap_entry(H, 1, K, V)), !.
insert(H, K, V) :- heap(H, S), not(heap_entry(H, _, _, V)),
	not(last_extracted(_)), P is S+1,
	retract(heap(H, S)), assert(heap(H, P)),
	assert(heap_entry(H, P, K, V)), shift_up(H, P), !.
insert(H, K, V) :- heap(H, S), not(heap_entry(H, _, _, V)),
	last_extracted(X), P is S+1,
	retract(heap(H, S)), assert(heap(H, P)),
	assert(heap_entry(H, X, K, V)), shift_up(H, X),
	retract(last_extracted(X)), !.

% Salva la posizione dell'ultimo elemento fittizio
% estratto
extract_pos(P) :- asserta(last_extracted(P)).

% Controlla che i padri siano più piccoli dei figli.
% Caso base: inserimento in testa.
shift_up(H, P) :- heap(H, _), P=<1, !.

% Caso base: figlio maggiore del padre.
shift_up(H, P) :- heap_entry(H, P, K, _), Parent is div(P, 2),
	heap_entry(H, Parent, ParentKey, _), K >= ParentKey, !.

% Passo ricorsivo: figlio minore del padre, scambio.
shift_up(H, P) :- heap_entry(H, P, K, V), Parent is div(P, 2),
	heap_entry(H, Parent, ParentKey, ParentValue),
	K < ParentKey, assert(heap_entry(H, Parent, K, V)),
	assert(heap_entry(H, P, ParentKey, ParentValue)),
	retract(heap_entry(H, P, K, V)),
	retract(heap_entry(H, Parent, ParentKey, ParentValue)),
	shift_up(H, Parent), !.

% Elenca l'albero e i nodi.
list_heap(H) :- listing(heap(H, _)), listing(heap_entry(H, _, _, _)).

% Estrae la testa dallo heap:
% Inserisce in testa un'entrata con chiave e valore fittizi.
extract(H, K, V) :- not_empty(H), heap_entry(H, 1, K, V),
	retract(heap_entry(H, 1, K, V)),
	assert(heap_entry(H, 1, 2e+60, valore_fittizio)),
	heapify(H, 1), !.

% Riordina lo heap e cancella il valore fittizio.
% Caso base: il nodo è una foglia (non ha figli).
% Se questa foglia ha il valore fittizio, cancellala.
heapify(H, P) :- heap(H, S), Child_L is P*2, Child_R is (P*2)+1,
	not(heap_entry(H, Child_L, _, _)),
	not(heap_entry(H, Child_R, _, _)),
	heap_entry(H, P, _, valore_fittizio),
	extract_pos(P),
	retract(heap_entry(H, P, _, valore_fittizio)),
	NuovaS is S-1, assert(heap(H, NuovaS)),
	retract(heap(H, S)), !.

heapify(H, P) :- heap(H, _), Child_L is P*2, Child_R is (P*2)+1,
	not(heap_entry(H, Child_L, _, _)),
	not(heap_entry(H, Child_R, _, _)), !.

% Passo ricorsivo: il nodo non ha figlio destro.
% Se il padre è più piccolo del figlio, fermati.
% Se il padre è più grande del figlio, scambia.
heapify(H, P) :- heap_entry(H, P, K, _), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_L, KL, _),
	not(heap_entry(H, Child_R, _, _)),
	K=<KL, !.

heapify(H, P) :- heap_entry(H, P, K, V), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_L, KL, VL),
	not(heap_entry(H, Child_R, _, _)),
	K>KL, assert(heap_entry(H, P, KL, VL)),
	assert(heap_entry(H, Child_L, K, V)),
	retract(heap_entry(H, P, K, V)),
	retract(heap_entry(H, Child_L, KL, VL)),
	heapify(H, Child_L), !.

% Passo ricorsivo: il nodo non ha figlio sinistro.
% Se il padre è più piccolo del figlio, fermati.
% Se il padre è più grande del figlio, scambia.
heapify(H, P) :- heap_entry(H, P, K, _), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_R, KR, _),
	not(heap_entry(H, Child_L, _, _)),
	K=<KR, !.

heapify(H, P) :- heap_entry(H, P, K, V), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_R, KR, VR),
	not(heap_entry(H, Child_L, _, _)),
	K>KR, assert(heap_entry(H, P, KR, VR)),
	assert(heap_entry(H, Child_R, K, V)),
	retract(heap_entry(H, P, K, V)),
	retract(heap_entry(H, Child_R, KR, VR)),
	heapify(H, Child_R), !.

% Passo ricorsivo: il nodo ha entrambi i figli, sx minore o uguale di dx.
% Se il padre è più piccolo di sx, fermati.
% Se il padre è più grande di sx, scambia.
heapify(H, P) :- heap_entry(H, P, K, _), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_L, KL, _),
	heap_entry(H, Child_R, KR, _), KL=<KR,
	K=<KL, !.

heapify(H, P) :- heap_entry(H, P, K, V), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_L, KL, VL),
	heap_entry(H, Child_R, KR, _), KL=<KR,
	assert(heap_entry(H, P, KL, VL)),
	assert(heap_entry(H, Child_L, K, V)),
	retract(heap_entry(H, P, K, V)),
	retract(heap_entry(H, Child_L, KL, VL)),
	heapify(H, Child_L), !.

% Passo ricorsivo: il nodo ha entrambi i figli, dx maggiore di sx.
% Se il padre è più piccolo di dx, fermati.
% Se il padre è più grande di dx, scambia.
heapify(H, P) :- heap_entry(H, P, K, _), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_L, KL, _),
	heap_entry(H, Child_R, KR, _), KR<KL,
	K<KR, !.

heapify(H, P) :- heap_entry(H, P, K, V), Child_L is P*2, Child_R is (P*2)+1,
	heap_entry(H, Child_L, KL, _),
	heap_entry(H, Child_R, KR, VR), KR<KL,
	assert(heap_entry(H, P, KR, VR)),
	assert(heap_entry(H, Child_R, K, V)),
	retract(heap_entry(H, P, K, V)),
	retract(heap_entry(H, Child_R, KR, VR)),
	heapify(H, Child_R), !.

% Modifica la chiave associata a V.
% Se la nuova chiave è più piccola della vecchia, controlla dal padre.
% Se la nuova chiave è più grande della vecchia, controlla i figli.
modify_key(H, NewKey, OldKey, V) :- heap_entry(H, P, OldKey, V),
	NewKey=<OldKey, assert(heap_entry(H, P, NewKey, V)),
	retract(heap_entry(H, P, OldKey, V)), shift_up(H, P), !.

modify_key(H, NewKey, OldKey, V) :- heap_entry(H, P, OldKey, V),
	NewKey>OldKey, assert(heap_entry(H, P, NewKey, V)),
	retract(heap_entry(H, P, OldKey, V)), heapify(H, P), !.

/* Cavriana Massimo
	Ultima modifica: 07/29/2014
*/
:- dynamic scheme/1.
:- dynamic userinfo/1.
:- dynamic host/1.
:- dynamic port/1.
:- dynamic path/1.
:- dynamic query/1.
:- dynamic fragment/1.

uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment) :-
	scheme(Scheme), userinfo(Userinfo), host(Host), port(Port),
	path(Path), query(Query), fragment(Fragment).

retract_old_uri :- retractall(scheme(_)), retractall(userinfo(_)),
	retractall(host(_)), retractall(port(_)),
	retractall(path(_)), retractall(query(_)),
	retractall(fragment(_)),
	assert(scheme([])), assert(userinfo([])),
	assert(host([])), assert(port([])), assert(path([])),
	assert(query([])), assert(fragment([])).

parsed_uri(URIString, uri(Sc, Ui, Ho, Po, Pa, Qu, Fr)) :- retract_old_uri,
	string_codes(URIString, URICodes), parse_scheme(URICodes, []),
	uri(Sc, Ui, Ho, Po, Pa, Qu, Fr).

is_not_special_char(Char) :- char_code('/', Char), !, fail.
is_not_special_char(Char) :- char_code('?', Char), !, fail.
is_not_special_char(Char) :- char_code('#', Char), !, fail.
is_not_special_char(Char) :- char_code('@', Char), !, fail.
is_not_special_char(Char) :- char_code(':', Char), !, fail.
is_not_special_char(Char) :- char_code(' ', Char), !, fail.
is_not_special_char(_).

parse_scheme([], _) :- !, fail.
parse_scheme([First, Second|Rest], Temp) :- is_not_special_char(First),
	char_code(:, Second), !, append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(scheme(_)),
	atom_codes(Scheme, Reverse_temp), assert(scheme(Scheme)),
	verify_scheme_syntax(Rest).
parse_scheme([First|Rest], Temp) :- is_not_special_char(First),
	append([First], Temp, New_temp), parse_scheme(Rest, New_temp), !.

verify_scheme_syntax(URICodes) :- scheme(Scheme),
	atom_codes(Scheme, [109, 97, 105, 108, 116, 111]), !,
	parse_mailto_userinfo(URICodes, []).
verify_scheme_syntax(URICodes) :- scheme(Scheme),
	atom_codes(Scheme, [110, 101, 119, 115]), !,
	parse_special_host(URICodes, []).
verify_scheme_syntax(URICodes) :- scheme(Scheme),
	atom_codes(Scheme, [116, 101, 108]), !,
	parse_telfax_userinfo(URICodes, []).
verify_scheme_syntax(URICodes) :- scheme(Scheme),
	atom_codes(Scheme, [102, 97, 120]), !,
	parse_telfax_userinfo(URICodes, []).
verify_scheme_syntax([First, Second|Rest]) :- char_code('/', First),
	char_code('/', Second), !, parse_authorithy(Rest, []).
verify_scheme_syntax([First|[]]) :- char_code('/', First), !.
verify_scheme_syntax([First, Second|Rest]) :- char_code('/', First),
	char_code('?', Second), !, parse_query(Rest, []).
verify_scheme_syntax([First, Second|Rest]) :- char_code('/', First),
	char_code('#', Second), !, parse_fragment(Rest, []).
verify_scheme_syntax([First|Rest]) :- char_code('/', First), !,
	parse_path(Rest, []).

parse_mailto_userinfo([], _) :- !, fail.
parse_mailto_userinfo([First, Second|Rest], Temp) :-
	is_not_special_char(First), char_code('@', Second), !,
	append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(userinfo(_)),
	atom_codes(Userinfo, Reverse_temp), assert(userinfo(Userinfo)),
	parse_special_host(Rest, []).
parse_mailto_userinfo([First|[]], Temp) :-
	is_not_special_char(First), append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp),  retractall(userinfo(_)),
	atom_codes(Userinfo, Reverse_temp), assert(userinfo(Userinfo)), !.
parse_mailto_userinfo([First|Rest], Temp) :-
	is_not_special_char(First), append([First], Temp, New_temp),
	parse_mailto_userinfo(Rest, New_temp), !.

parse_special_host([], _) :- !, fail.
parse_special_host([First|_], _) :-
	char_code('.', First), !, fail.
parse_special_host([First, Second|Rest], Temp) :-
	is_not_special_char(First), char_code('.', Second), !,
	append([Second, First], Temp, New_temp),
	parse_special_host(Rest, New_temp).
parse_special_host([First|[]], Temp) :- is_not_special_char(First),
	append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)), !.
parse_special_host([First|Rest], Temp) :-
	is_not_special_char(First), append([First], Temp, New_temp),
	parse_special_host(Rest, New_temp), !.

parse_telfax_userinfo([], _) :- !, fail.
parse_telfax_userinfo([First|[]], Temp) :- is_not_special_char(First),
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(userinfo(_)), string_codes(Userinfo, Reverse_temp),
	assert(userinfo(Userinfo)), !.
parse_telfax_userinfo([First|Rest], Temp) :- is_not_special_char(First),
	append([First], Temp, New_temp),
	parse_telfax_userinfo(Rest, New_temp), !.

parse_authorithy([], _) :- !, fail.
parse_authorithy([First|[]], Temp) :- is_not_special_char(First),
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(host(_)), atom_codes(Host, Reverse_temp),
	assert(host(Host)), !.
parse_authorithy([First, Second|Rest], Temp) :- is_not_special_char(First),
	char_code('@', Second), !, append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(userinfo(_)),
	atom_codes(Userinfo, Reverse_temp), assert(userinfo(Userinfo)),
	parse_host(Rest, []).
parse_authorithy([First, Second|Rest], Temp) :- is_not_special_char(First),
	char_code(':', Second), !, append([First], Temp, New_temp),
	dots_check(New_temp), reverse(New_temp, Reverse_temp),
	retractall(host(_)), atom_codes(Host, Reverse_temp),
	assert(host(Host)), parse_port(Rest, []).
parse_authorithy([First, Second|[]], Temp) :-
	is_not_special_char(First), char_code('/', Second), !,
	append([First], Temp, New_temp),
	dots_check(New_temp), reverse(New_temp, Reverse_temp),
	retractall(host(_)), atom_codes(Host, Reverse_temp),
	assert(host(Host)).
parse_authorithy([First, Second, Third|Rest], Temp) :-
	is_not_special_char(First), char_code('/', Second),
	char_code('?', Third), !, append([First], Temp, New_temp),
	dots_check(New_temp), reverse(New_temp, Reverse_temp),
	retractall(host(_)), atom_codes(Host, Reverse_temp),
	assert(host(Host)), parse_query(Rest, []).
parse_authorithy([First, Second, Third|Rest], Temp) :-
	is_not_special_char(First), char_code('/', Second),
	char_code('#', Third), !, append([First], Temp, New_temp),
	dots_check(New_temp), reverse(New_temp, Reverse_temp),
	retractall(host(_)), atom_codes(Host, Reverse_temp),
	assert(host(Host)), parse_fragment(Rest, []).
parse_authorithy([First, Second|Rest], Temp) :-
	is_not_special_char(First), char_code('/', Second), !,
	append([First], Temp, New_temp), dots_check(New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)),
	parse_path(Rest, []).
parse_authorithy([First|Rest], Temp) :- is_not_special_char(First),
	append([First], Temp, New_temp), parse_authorithy(Rest, New_temp), !.

dots_check([]) :- !.
dots_check([First, Second|_]) :- char_code('.', First),
	char_code('.', Second), !, fail.
dots_check([_|Rest]) :- dots_check(Rest), !.

parse_path([], _) :- !, fail.
parse_path([First, Second|[]], Temp) :-
	is_not_special_char(First), char_code('/', Second), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(path(_)), atom_codes(Path, Reverse_temp),
	assert(path(Path)).
parse_path([First, Second|Rest], Temp) :-
	is_not_special_char(First), char_code('/', Second), !,
	append([Second, First], Temp, New_temp), parse_path(Rest, New_temp).
parse_path([First, Second|Rest], Temp) :-
	is_not_special_char(First), char_code('?', Second), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(path(_)), atom_codes(Path, Reverse_temp),
	assert(path(Path)), parse_query(Rest, []).
parse_path([First, Second|Rest], Temp) :-
	is_not_special_char(First), char_code('#', Second), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(path(_)), atom_codes(Path, Reverse_temp),
	assert(path(Path)), parse_fragment(Rest, []).
parse_path([First|[]], Temp) :- is_not_special_char(First), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(path(_)), atom_codes(Path, Reverse_temp),
	assert(path(Path)).
parse_path([First|Rest], Temp) :- is_not_special_char(First), !,
	append([First], Temp, New_temp), parse_path(Rest, New_temp).

is_not_query_char(Char) :- char_code(' ', Char), !, fail.
is_not_query_char(Char) :- char_code('#', Char), !, fail.
is_not_query_char(_).

parse_query([], _) :- !, fail.
parse_query([First, Second|Rest], Temp) :-
	is_not_query_char(First), char_code('#', Second), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(query(_)), atom_codes(Query, Reverse_temp),
	assert(query(Query)), parse_fragment(Rest, []).
parse_query([First|[]], Temp) :- is_not_query_char(First), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(query(_)), atom_codes(Query, Reverse_temp),
	assert(query(Query)).
parse_query([First|Rest], Temp) :- is_not_query_char(First), !,
	append([First], Temp, New_temp), parse_query(Rest, New_temp).

parse_fragment([], _) :- !, fail.
parse_fragment([First|[]], Temp) :-
	not(char_code(' ', First)), !, append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(fragment(_)),
	atom_codes(Fragment, Reverse_temp), assert(fragment(Fragment)).
parse_fragment([First|Rest], Temp) :-
	not(char_code(' ', First)), !, append([First], Temp, New_temp),
	parse_fragment(Rest, New_temp).

parse_port([], _) :- !, fail.
parse_port([First|[]], Temp) :- number_codes(_, [First]), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(port(_)), atom_codes(Port, Reverse_temp),
	assert(port(Port)).
parse_port([First, Second, Third|Rest], Temp) :- number_codes(_, [First]),
	char_code('/', Second),	char_code('?', Third), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(port(_)), atom_codes(Port, Reverse_temp),
	assert(port(Port)), parse_query(Rest, []).
parse_port([First, Second, Third|Rest], Temp) :- number_codes(_, [First]),
	char_code('/', Second),	char_code('#', Third), !,
	append([First], Temp, New_temp), reverse(New_temp, Reverse_temp),
	retractall(port(_)), atom_codes(Port, Reverse_temp),
	assert(port(Port)), parse_fragment(Rest, []).
parse_port([First, Second|[]], Temp) :- number_codes(_, [First]),
	char_code('/', Second), !, append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(port(_)),
	atom_codes(Port, Reverse_temp), assert(port(Port)).
parse_port([First, Second|Rest], Temp) :- number_codes(_, [First]),
	char_code('/', Second), !, append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(port(_)),
	atom_codes(Port, Reverse_temp), assert(port(Port)),
	parse_path(Rest, []).
parse_port([First|Rest], Temp) :- number_codes(_, [First]), !,
	append([First], Temp, New_temp), parse_port(Rest, New_temp).

parse_host([], _) :- !, fail.
parse_host([First|[]], Temp) :- is_not_special_char(First),
	not(char_code('.', First)), !, append([First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)).
parse_host([First, Second|Rest], Temp) :-
	is_not_special_char(First), not(char_code('.', First)),
	char_code('.', Second), !,
	append([Second, First], Temp, New_temp),
	parse_host(Rest, New_temp).
parse_host([First, Second|Rest], Temp) :-
	is_not_special_char(First), not(char_code('.', First)),
	char_code(':', Second), !,
	append([Second, First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)),
	parse_port(Rest, []).
parse_host([First, Second, Third|Rest], Temp) :-
	is_not_special_char(First), not(char_code('.', First)),
	char_code('/', Second), char_code('?', Third), !,
	append([Second, First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)),
	parse_query(Rest, []).
parse_host([First, Second, Third|Rest], Temp) :-
	is_not_special_char(First), not(char_code('.', First)),
	char_code('/', Second), char_code('#', Third), !,
	append([Second, First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)),
	parse_fragment(Rest, []).
parse_host([First, Second|[]], Temp) :-
	is_not_special_char(First), not(char_code('.', First)),
	char_code('/', Second), !,
	append([Second, First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)).
parse_host([First, Second|Rest], Temp) :-
	is_not_special_char(First), not(char_code('.', First)),
	char_code('/', Second), !,
	append([Second, First], Temp, New_temp),
	reverse(New_temp, Reverse_temp), retractall(host(_)),
	atom_codes(Host, Reverse_temp), assert(host(Host)),
	parse_path(Rest, []).
parse_host([First|Rest], Temp) :- is_not_special_char(First),
	not(char_code('.', First)), !, append([First], Temp, New_temp),
	parse_host(Rest, New_temp).





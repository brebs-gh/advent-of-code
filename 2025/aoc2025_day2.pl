% Written in SWI-Prolog
% https://adventofcode.com/2025/day/2
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

invalid_ids_sum(SumP1, SumP2) :-
	once(phrase_from_file(invalid_id_check, 'aoc2025_day2_input.txt')),
	setof(N, inv_p1(N), SP1),
	sumlist(SP1, SumP1),
	setof(N, inv_p2(N), SP2),
	sumlist(SP2, SumP2).

invalid_id_check -->
	% Start and End
	digs(S), "-", digs(E),
	{	check_range(S, E) },
	invalid_id_check_next.

% At least 1 digit
digs([H|T]) --> digs_([H|T]).

digs_([H|T]) --> dig(H), digs_(T).
digs_([]) --> [].

dig(D) --> [D], { between(0'0, 0'9, D) }.

invalid_id_check_next --> [10].
invalid_id_check_next --> ",", invalid_id_check.

check_range(S, E) :-
	% Lower and Upper
	number_codes(L, S),
	number_codes(U, E),
	between(L, U, N),
	number_codes(N, Cs),
	% Still check p2 if p1 failed
	ignore(pattern_p1(Cs, N)),
	ignore(pattern_p2(Cs, N)),
	% Keep searching in the between
	fail.
% Is success, after searching
check_range(_S, _E).

pattern_p1(Cs, N) :-
	% Two halves
	append(A, A, Cs),
	assertz(inv_p1(N)).

pattern_p2(Cs, N) :-
	repeats(Cs, _),
	assertz(inv_p2(N)).

repeats(L, Start) :-
	% Ensuring not empty lists
	L = [_|_],
	Start = [_|_],
	repeats_(L, Start).

repeats_(L, Start) :-
	append(Start, Start, L).
repeats_(L, Start) :-
	append(Start, Rem, L),
	repeats(Rem, Start).

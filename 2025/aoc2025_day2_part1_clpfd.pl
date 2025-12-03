% Written in SWI-Prolog
% https://adventofcode.com/2025/day/2
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

% Just part 1, using clpfd
% Part 2 was not suitable for clpfd, so switched to list processing

:- use_module(library(clpfd)).

invalid_ids_sum(Sum) :-
	once(phrase_from_file(invalid_id_check, 'aoc2025_day2_input.txt')),
	setof(N, inv(N), S),
	sumlist(S, Sum).

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
	% Using clpfd
	N in L..U,
	% Limit to a reasonable range
	P in 0..15,
	I in 1..U,
	% Let clpfd optimize
	% Is #> to exclude 110
	10 ^ P #> I,
	10 ^ (P - 1) #=< I,
	N #= ((10 ^ P) * I) + I,
	% Label in order of minimal range, for performance
	label([P, I, N]),
	assertz(inv(N)),
	% Keep searching
	fail.
% Is success, after searching
check_range(_S, _E).

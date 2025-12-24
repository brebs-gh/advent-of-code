% Written in SWI-Prolog
% https://adventofcode.com/2025/day/3
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

% A slightly more relational solution
% Runs in under 0.3 seconds

:- use_module(library(clpq)).

day3(P1, P2) :-
	once(phrase_from_file(jolts, 'aoc2025_day3_input.txt')),
	findall(N, battery(p1, N), P1s),
	sum_list(P1s, P1),
	findall(N, battery(p2, N), P2s),
	sum_list(P2s, P2).

jolts -->
	digs(L),
	[10],
	{	select_batteries(L) },
	jolts_next.

jolts_next --> [].
jolts_next --> jolts.

% At least 1 digit
digs([H|T]) --> digs_([H|T]).

digs_([H|T]) --> dig(H), digs_(T).
digs_([]) --> [].

dig(D) --> [D], { between(0'0, 0'9, D) }.

select_batteries(L) :-
	select_batteries(p1, 2, L),
	select_batteries(p2, 12, L).

select_batteries(Part, BC, L) :-
	select_batteries_(BC, L, Bs),
	!,
	number_codes(N, Bs),
	assertz(battery(Part, N)).

select_batteries_(0, _L, []).
select_batteries_(BC, L, [B|Bs]) :-
	BC > 0,
	RC is BC - 1,
	remove_last_n(RC, L, Last, L0),
	select_elem_before_after(L0, B, Before, After),
	% Choose the first biggest
	maplist(lt_clpq(B), Before),
	maplist(lte_clpq(B), After),
	% Reassemble remaining list
	append(After, Last, Rem),
	select_batteries_(RC, Rem, Bs).

remove_last_n(N, L, Last, L0) :-
	length(Last, N),
	append(L0, Last, L).

select_elem_before_after([H|T], Elem, Before, After) :-
    select_elem_before_after_(T, H, Elem, Before, After).

select_elem_before_after_(T, H, H, [], T).
select_elem_before_after_([H|T], B, E, [B|Before], After) :-
    select_elem_before_after_(T, H, E, Before, After).

lt_clpq(N, Lt) :-
	{ Lt < N }.

lte_clpq(N, Lte) :-
	{ Lte =< N }.

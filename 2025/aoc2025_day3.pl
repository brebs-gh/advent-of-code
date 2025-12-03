% Written in SWI-Prolog
% https://adventofcode.com/2025/day/3
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day3(P1, P2) :-
	once(phrase_from_file(jolts, 'aoc2025_day3_input.txt')),
	findall(N, battery(p1, N), P1s),
	sumlist(P1s, P1),
	findall(N, battery(p2, N), P2s),
	sumlist(P2s, P2).

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

select_batteries_(0, _L, Bs) :-
	!,
	Bs = [].
select_batteries_(BC, L, [B|Bs]) :-
	RC is BC - 1,
	remove_last_n(RC, L, Last, L0),
	select_forward(B, L0, F),
	% Choose the biggest
	none_bigger(B, F),
	% Reassemble remaining list
	append(F, Last, Rem),
	!,
	select_batteries_(RC, Rem, Bs).

none_bigger(E, L) :-
	(	member(B, L),
		B @> E
	->	fail
	;	true
	).

remove_last_n(N, L, Last, L0) :-
	length(Last, N),
	append(L0, Last, L).

% A variation on select/3 which only goes forward through the list
select_forward(E, [H|T], F) :-
    select_forward_(T, H, E, F).

select_forward_(T, H, H, T).
select_forward_([H|T], _, E, F) :-
    select_forward_(T, H, E, F).

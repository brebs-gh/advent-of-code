% Written in SWI-Prolog
% https://adventofcode.com/2025/day/12
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

% Would have been interesting: domino tiling: https://www.youtube.com/watch?v=h5Xy4YjCZxM

% Day 12 has no part 2
day12(P1) :-
	once(phrase_from_file(presents, 'aoc2025_day12_input.txt')),
	part1(P1).

presents --> integer(PId), ":", [10], shape(S),
	{ !, store_present(PId, S) },
	( presents | regions ).

shape([]) --> [10].
shape([H|T]) --> shape_line(H), shape(T).

shape_line([]) --> [10].
shape_line([0|T]) --> ".", shape_line(T).
shape_line([1|T]) --> "#", shape_line(T).

regions --> integer(W), "x", integer(H), ": ",
	seq_delim(integer, " ", Qs), [10],
	{	!, assertz(present_region(W, H, Qs)) },
	( [] | regions ).

seq_delim(GoalName, Delim, [H|T]) --> call(GoalName, H),
	( Delim, !, seq_delim(GoalName, Delim, T) | [], { T = [] } ).

integer(I) --> digits([H|T]), { number_codes(I, [H|T]) }.

digits([D|Ds]) --> [D], { between(0'0, 0'9, D) }, digits(Ds).
digits([]) --> [].

store_present(PId, S) :-
	assertz(present(PId, S)),
	aggregate_all(count, (member(Row, S), member(E, Row), square_fill(E)), Hashes),
	assertz(present_hashes(PId, Hashes)).

square_fill(1).

% This was a terribly misleading question - the complexities can be ignored
% https://www.reddit.com/r/adventofcode/comments/1pkjynl/2025_day_12_day_12_solutions/
part1(P1) :-
	aggregate_all(count, present_region_possible, P1).

present_region_possible :-
	present_region(W, H, Qs),
	RegSize is W * H,
	% Don't need to bother with rearrangements
	present_region_sizes(Qs, S, _Min),
	RegSize >= S.

present_region_sizes(Qs, S, S0) :-
	sum_list(Qs, Tot),
	% All the same size
	S is Tot * 9,
	aggregate_all(sum(M), present_min_sizes(Qs, 1, M), S0).

present_min_sizes([], _, 0).
present_min_sizes([H|T], PId, S) :-
	(	H = 0
	->	S1 = 0
	;	present_hashes(PId, Hashes),
		S1 is Hashes * H
	),
	PId1 is PId + 1,
	present_min_sizes(T, PId1, S0),
	S is S1 + S0.

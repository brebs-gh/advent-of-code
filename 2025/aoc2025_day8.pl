% Written in SWI-Prolog
% https://adventofcode.com/2025/day/8
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day8(P1, P2) :-
	once(phrase_from_file(junctions(Ps), 'aoc2025_day8_input.txt')),
	distance_pairs_sort(Ps, DPs),
	part1(DPs, P1).
	%part2(Ps, P2).

part1(DPs, P1) :-
	length(DPs10, 1000),
	once(append(DPs10, _, DPs)),
	forall(member(_-pair(P1, P2), DPs10),
		assertz(dp(P1, P2))
	),
	circuit_sizes,
	findall(S, circuit_size(S), Sizes),
	msort(Sizes, SizesSort),
	reverse(SizesSort, SizesSortR),
	SizesSortR = [S1, S2, S3|_],
	P1 is S1 * S2 * S3.

select_same_circuit([], []) :- !.
select_same_circuit(Ps, [P|PsC]) :-
	select(P, Ps).
	% TODO

junctions([P|Ps]) --> point(P), junctions_next(Ps).

junctions_next([]) --> [].
junctions_next(Ps) --> junctions(Ps).

point(p(X, Y, Z)) --> integer(X), ",", integer(Y), ",", integer(Z), [10].

integer(I) --> digits([H|T]), { number_codes(I, [H|T]) }.

digits([D|Ds]) --> [D], { between(0'0, 0'9, D) }, digits(Ds).
digits([]) --> [].

distance(p(X1, Y1, Z1), p(X2, Y2, Z2), Dist) :-
	Dist is sqrt((X1 - X2)^2 + (Y1 - Y2)^2 + (Z1 - Z2)^2).

distance_pair(Ps, Dist-pair(P1, P2)) :-
	select_forward(P1, Ps, Ps0),
	member(P2, Ps0),
	distance(P1, P2, Dist).

distance_pairs_sort(Ps, DPs) :-
	findall(DP, distance_pair(Ps, DP), DPsU),
	keysort(DPsU, DPs).

circuit_sizes :-
	(	% Take a short path
		retract(dp(P1, P2))
	->	retractall(in_circuit(_)),
		% Include the individual junction points
		assertz(in_circuit(P1)),
		assertz(in_circuit(P2)),
		% Count the connecting points
		in_circuit_size(Count),
		assertz(circuit_size(Count)),
		circuit_sizes
	;	true
	).

in_circuit_size(Count) :-
	% Take a junction point
	retract(in_circuit(P)),
	% Include the new connections to this junction point
	forall(
		(	retract(dp(P, P2))
		;	retract(dp(P2, P))
		),
		% Include P2 if new
		(	in_circuit(P2)
		->	true
		;	assertz(in_circuit(P2))
		)
	),
	!,
	% Loop through the pool of points, looking for new connections
	in_circuit_size(Count0),
	Count is 1 + Count0.
in_circuit_size(0).

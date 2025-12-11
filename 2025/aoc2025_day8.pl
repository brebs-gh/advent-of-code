% Written in SWI-Prolog
% https://adventofcode.com/2025/day/8
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day8(P1, P2) :-
	once(phrase_from_file(junctions(Ps), 'aoc2025_day8_input.txt')),
	distance_pairs_sort(Ps, DPs),
	part1(DPs, P1),
	% Setup
	forall(member(_-pair(J1, J2), DPs),
		% Don't need the Distance - just the order is sufficient
		assertz(pairable_short(J1, J2))
	),
	forall(member(J, Ps),
		assertz(unconnected_junction(J))
	),
	% Seed with the shortest pair
	start_single_circuit,
	connect_closest_unconnected_pairs,
	% Retrieve the last 2
	b_getval(last2, Last2),
	maplist(junction_x_coord, Last2, [X1, X2]),
	P2 is X1 * X2.

junction_x_coord(p(X, _, _), X).

part1(DPs, P1) :-
	length(DPs10, 1000),
	once(append(DPs10, _, DPs)),
	forall(member(_-pair(P1, P2), DPs10),
		assertz(dp(P1, P2))
	),
	circuit_sizes,
	findall(S, circuit_size(S), Sizes),
	msort(Sizes, SizesSort),
	once(append(_, [S1, S2, S3], SizesSort)),
	P1 is S1 * S2 * S3.

start_single_circuit :-
	pairable_short(J1, J2),
	retract(pairable_short(J1, J2)),
	retract(unconnected_junction(J1)),
	retract(unconnected_junction(J2)),
	% Remove unwanted choicepoints
	!.

connect_closest_unconnected_pairs :-
	(	pairable_short(J1, J2),
		once(
			(	unconnected_junction(J1)
			->	Juncs = [J1, J2]
			;	unconnected_junction(J2)
			->	Juncs = [J2, J1]
			)
		)
	->	% Connect this short pair
		retract(pairable_short(J1, J2)),
		Juncs = [JUnconnected, _JAlreadyConnected],
		% Make the connection
		retract(unconnected_junction(JUnconnected)),
		% Remove unwanted choicepoints
		!,
		% Remember the last 2 connections
		nb_setval(last2, Juncs),
		% Loop
		connect_closest_unconnected_pairs
	;	% Ensure all the junctions have been connected
		\+ unconnected_junction(_)
	).

junctions([P|Ps]) --> point(P), junctions_next(Ps).

junctions_next([]) --> [].
junctions_next(Ps) --> junctions(Ps).

point(p(X, Y, Z)) --> integer(X), ",", integer(Y), ",", integer(Z), [10].

integer(I) --> digits([H|T]), { number_codes(I, [H|T]) }.

digits([D|Ds]) --> [D], { between(0'0, 0'9, D) }, digits(Ds).
digits([]) --> [].

distance(p(X1, Y1, Z1), p(X2, Y2, Z2), Dist) :-
	% Might as well include the sqrt - isn't needed for this puzzle
	Dist is sqrt((X1 - X2)^2 + (Y1 - Y2)^2 + (Z1 - Z2)^2).

distance_pair(Ps, Dist-pair(P1, P2)) :-
	select_forward(P1, Ps, Ps0),
	member(P2, Ps0),
	distance(P1, P2, Dist).

distance_pairs_sort(Ps, DPs) :-
	findall(DP, distance_pair(Ps, DP), DPsU),
	keysort(DPsU, DPs).

% A variation on select/3 which only goes forward through the list
select_forward(E, [H|T], F) :-
    select_forward_(T, H, E, F).

select_forward_(T, H, H, T).
select_forward_([H|T], _, E, F) :-
    select_forward_(T, H, E, F).

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

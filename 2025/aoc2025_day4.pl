% Written in SWI-Prolog
% https://adventofcode.com/2025/day/4
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day4(P1, P2) :-
	once(phrase_from_file(diagram(pos(1, 1)), 'aoc2025_day4_input.txt')),
	aggregate_all(count, forklift_accessible_paper(_), P1),
	aggregate_all(count, paper(_), Papers),
	remove_accessible_papers,
	aggregate_all(count, paper(_), Papers0),
	% How many papers were removed
	P2 is Papers - Papers0.

% Row and Column
diagram(pos(R, C)) -->
	"@",
	{	assertz(paper(pos(R, C))) },
	diagram_next(pos(R, C)).
diagram(pos(R, C)) -->
	".",
	diagram_next(pos(R, C)).
diagram(pos(R, _)) -->
	[10],
	{	R1 is R + 1 },
	diagram_next(pos(R1, 0)).

diagram_next(pos(R, C)) -->
	{	C1 is C + 1 },
	diagram_next_(pos(R, C1)).

diagram_next_(_) --> [].
diagram_next_(Pos) --> diagram(Pos).

adjacent(pos(R, C), pos(R-1, C-1)).
adjacent(pos(R, C), pos(R-1, C)).
adjacent(pos(R, C), pos(R-1, C+1)).
adjacent(pos(R, C), pos(R, C-1)).
adjacent(pos(R, C), pos(R, C+1)).
adjacent(pos(R, C), pos(R+1, C-1)).
adjacent(pos(R, C), pos(R+1, C)).
adjacent(pos(R, C), pos(R+1, C+1)).

paper_adjacent(Pos) :-
	adjacent(Pos, pos(RM, CM)),
	% Perform the maths
	R is RM,
	C is CM,
	paper(pos(R, C)).

forklift_accessible_paper(Pos) :-
	paper(Pos),
	aggregate_all(count, paper_adjacent(Pos), Adjs),
	Adjs < 4.

remove_accessible_papers :-
	(	forklift_accessible_paper(_)
	->	forall(forklift_accessible_paper(Pos),
			retract(paper(Pos))
		),
		% More papers might now be accessible to remove
		remove_accessible_papers
	;	% No more papers can be removed
		true
	).

% Written in SWI-Prolog
% https://adventofcode.com/2025/day/9
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

% Runs in 3 seconds

day9(P1, P2) :-
	once(phrase_from_file(coords, 'aoc2025_day9_input.txt')),
	part1(P1),
	part2(P2).

coords --> word(XCs), ",", word(YCs), [10],
	{	number_codes(X, XCs),
		number_codes(Y, YCs),
		assertz(coords(c(X, Y)))
	},
	coords_next.

coords_next --> coords.
coords_next --> [].

% At least 1 char
word([H|T]) --> word_([H|T]).

word_(L), R --> [C], { bad_word_char(C), !, R = [C], L = [] }.
word_([H|T]) --> [H], word_(T).

% Includes newline codes 10 and 13
bad_word_char(C) :- code_type(C, space).
bad_word_char(0',).

part1(P1) :-
	findall(RedTile, coords(RedTile), RedTiles),
	findall(Area-pair(RT1, RT2),
		(	select_two(RT1, RT2, RedTiles),
			red_tile_area(RT1, RT2, Area)
		),
		Areas
	),
	keysort(Areas, AreasS),
	reverse(AreasS, AreasSR),
	% Answer to part 1
	AreasSR = [P1-_|_],
	% Prepare for part 2
	forall(member(Area-Pair, AreasSR),
		(	pair_top_left_bottom_right(Pair, PairTLBR),
			assertz(area_large(Area, PairTLBR))
		)
	).

red_tile_area(c(X1, Y1), c(X2, Y2), Area) :-
	Area is (abs(X1 - X2) + 1) * (abs(Y1 - Y2) + 1).

part2(P2) :-
	findall(RedTile, coords(RedTile), RedTiles),
	RedTiles = [H|T],
	% "The list wraps, so the first red tile is also connected to the last red tile"
	append(T, [H], RedTilesWrap),
	% Find the maximum satisfactory area
	area_large(Area, Pair),
	% Cannot have green tile lines going through an Area rectangle
	% This is assuming that lines will never be next to each other
	no_lines_conflicting_with_pair(RedTilesWrap, Pair),
	% Stop when found the first solution, which is the maximum
	!,
	P2 = Area.

no_lines_conflicting_with_pair([_], _).
no_lines_conflicting_with_pair([H1, H2|T], Pair) :-
	(	line_pair_conflict(pair(H1, H2), Pair)
	->	false
	;	no_lines_conflicting_with_pair([H2|T], Pair)
	).

lower_upper(X, Y, L, U) :-
	(	X @< Y
	->	(L, U) = (X, Y)
	;	(L, U) = (Y, X)
	).

line_pair_conflict(Line, PairTLBR) :-
	Line = pair(c(X1, Y1), c(X2, Y2)),
	(	X1 == X2
	->	AxisDif = y
	;	Y1 == Y2
	->	AxisDif = x
	),
	line_pair_conflict_axis(AxisDif, Line, PairTLBR).

line_pair_conflict_axis(x, pair(c(X1, Y1), c(X2, _Y2)), pair(c(XTL, YTL), c(XBR, YBR))) :-
	% Is on X-axis
	Y1 > YBR,
	Y1 < YTL,
	lower_upper(X1, X2, XL, XU),
	(	% Intersects left of rectangle
		XL < XTL, XU > XTL
	;	% Intersects right of rectangle
		XL < XBR, XU > XBR
	;	% Starts and ends inside
		XL > XTL, XU < XBR
	).
line_pair_conflict_axis(y, pair(c(X1, Y1), c(_X2, Y2)), pair(c(XTL, YTL), c(XBR, YBR))) :-
	% Is on Y-axis
	X1 > XBR,
	X1 < XTL,
	lower_upper(Y1, Y2, YL, YU),
	(	% Intersects bottom of rectange
		YL < YBR, YU > YBR
	;	% Intersects top of rectangle
		YL < YTL, YU > YTL
	;	% Starts and ends inside
		YL > YBR, YU < YTL
	).

pair_top_left_bottom_right(pair(c(X1, Y1), c(X2, Y2)), pair(c(XTL, YTL), c(XBR, YBR))) :-
	lower_upper(X1, X2, XTL, XBR),
	lower_upper(Y1, Y2, YBR, YTL).

select_two(E1, E2, L) :-
	select_forward(E1, L, L0),
	member(E2, L0).

% A variation on select/3 which only goes forward through the list
select_forward(E, [H|T], F) :-
    select_forward_(T, H, E, F).

select_forward_(T, H, H, T).
select_forward_([H|T], _, E, F) :-
    select_forward_(T, H, E, F).

% Written in SWI-Prolog
% https://adventofcode.com/2025/day/9
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day9(P1, P2) :-
	once(phrase_from_file(coords, 'aoc2025_day9_input.txt')),
	writeln(parsed),
	part1(P1).

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
	aggregate_all(max(Area), (
		select_two(RT1, RT2, RedTiles),
		red_tile_area(RT1, RT2, Area)
	), P1).

red_tile_area(c(X1, Y1), c(X2, Y2), Area) :-
	Area is (abs(X1 - X2) + 1) * (abs(Y1 - Y2) + 1).

select_two(E1, E2, L) :-
	select_forward(E1, L, L0),
	member(E2, L0).

% A variation on select/3 which only goes forward through the list
select_forward(E, [H|T], F) :-
    select_forward_(T, H, E, F).

select_forward_(T, H, H, T).
select_forward_([H|T], _, E, F) :-
    select_forward_(T, H, E, F).


% https://adventofcode.com/2020/day/3
% https://www.reddit.com/r/prolog/comments/1unqdpj/i_want_to_know_if_this_claude_generated_code/

% ?- time(day3(P1, P2)).
% 48,779 inferences, 0.008 CPU in 0.008 seconds (100% CPU, 5965119 Lips)


day3(Part1, Part2) :-
	parse_input,
	part1(Part1),
	part2(Part2).


parse_input :-
	once(phrase_from_file(toboggan_map(0, 0), 'advent2020/day3_toboggan.txt')).


:- dynamic map_width/1.

toboggan_map(Row, Col) --> ".",
	toboggan_map_next_col(Row, Col).
toboggan_map(Row, Col) --> "#",
	{	assertz(tree_row_col(Row, Col)) },
	toboggan_map_next_col(Row, Col).
toboggan_map(Row, Col) --> "\n",
	{	(	map_width(Width)
		->	Col == Width
		;	assertz(map_width(Col))
		),
		Row1 is Row + 1
	},
	toboggan_map(Row1, 0).
toboggan_map(Row, _Col) --> [],
	{	assertz(map_rows(Row)) }.

toboggan_map_next_col(Row, Col) -->
	{	Col1 is Col + 1 },
	toboggan_map(Row, Col1).


start_row_col(0, 0).

% 2nd arg is a "slope" number, to separate the counts per slope
route_row_col_increments(part1, 1, rc(1, 3)).
route_row_col_increments(part2, 1, rc(1, 1)).
route_row_col_increments(part2, 2, rc(1, 3)).
route_row_col_increments(part2, 3, rc(1, 5)).
route_row_col_increments(part2, 4, rc(1, 7)).
route_row_col_increments(part2, 5, rc(2, 1)).


route_row_col(_RowInc, _ColInc, Row, Col) :-
	start_row_col(Row, Col).
route_row_col(RowInc, ColInc, Row, Col) :-
	start_row_col(PrevRow, PrevCol),
	route_row_col_loop(RowInc, ColInc, PrevRow, PrevCol, Row, Col).


route_row_col_loop(RowInc, ColInc, PrevRow, PrevCol, Row, Col) :-
	ThisRow is PrevRow + RowInc,
	map_rows(Rows),
	ThisRow < Rows,
	ThisColToWrap is PrevCol + ColInc,
	wrap_col(ThisColToWrap, ThisCol),
	(	(Row, Col) = (ThisRow, ThisCol)
	;	route_row_col_loop(RowInc, ColInc, ThisRow, ThisCol, Row, Col)
	).


wrap_col(Wrap, Col) :-
	map_width(Width),
	% First column of Wrap is zero
	(	Wrap >= Width
	->	Col is Wrap - Width
	;	Col is Wrap
	).


route_tree_row_col(RowInc, ColInc, Row, Col) :-
	route_row_col(RowInc, ColInc, Row, Col),
	tree_row_col(Row, Col).


route_count_mult(Part, Mult) :-
	findall(Trees,
		(	route_row_col_increments(Part, _Slope, rc(RowInc, ColInc)),
			aggregate_all(count, route_tree_row_col(RowInc, ColInc, _Row, _Col), Trees)
		),
		TreeCounts
	),
	foldl(multiply, TreeCounts, 1, Mult).


part1(Trees) :-
	route_count_mult(part1, Trees).


part2(Trees) :-
	route_count_mult(part2, Trees).

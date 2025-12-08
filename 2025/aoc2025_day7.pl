% Written in SWI-Prolog
% https://adventofcode.com/2025/day/7
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day7(P1, P2) :-
	assert(count_cols(_)),
	once(phrase_from_file(tachyon, 'aoc2025_day7_input.txt')),
	setof(Row-Col, Direction^splitter_splits_beam(Row, Col, Direction), S),
	length(S, P1),
	start(SRow, SCol),
	SRow1 is SRow + 1,
	beam_splits_to_bottom(SRow1, SCol, P2).

tachyon --> tachyon_rows(1).

tachyon_rows(Rows, [], []) :-
	Rows0 is Rows - 1,
	assertz(count_rows(Rows0)).
tachyon_rows(Row) --> tachyon_cols(Row, 1).

tachyon_cols(Row, Col) -->
	".",
	tachyon_col_next(Row, Col).
tachyon_cols(Row, Col) -->
	"S",
	{	assertz(start(Row, Col)) },
	tachyon_col_next(Row, Col).
tachyon_cols(Row, Col) -->
	"^",
	{	assertz(splitter(Row, Col)) },
	tachyon_col_next(Row, Col).

tachyon_col_next(Row, Col) -->
	{ Col1 is Col + 1 },
	tachyon_col_next_(Row, Col1).

tachyon_col_next_(Row, Col) -->
	[10],
	!,
	{	Col0 is Col - 1,
		count_cols(Cols),
		(	ground(Cols)
		->	Cols == Col0
		;	retract(count_cols(_)),
			assertz(count_cols(Col0))
		),
		Row1 is Row + 1
	},
	tachyon_rows(Row1).
tachyon_col_next_(Row, Col) --> tachyon_cols(Row, Col).

% Tabling here is only very slightly beneficial
:- table splitter_down_from_start/4.
splitter_down_from_start(Row, SRow, Col) :-
	splitter(SPRow, Col),
	SPRow < Row,
	SPRow > SRow.

% Tabling here is only very slightly beneficial
:- table splitter_blocking/3.
splitter_blocking(SPRow, Row, Col) :-
	splitter(BRow, Col),
	BRow > SPRow,
	BRow < Row.

% On reflection, this is not an elegant method for Part 1!
row_col_has_beam(Row, Col, [start(Row, Col)]) :-
	start(Row, Col).
row_col_has_beam(Row, Col, Path) :-
	start(SRow, SCol),
	Row > SRow,
	(	% Could go straight down from Start
		Col = SCol,
		\+ splitter_down_from_start(Row, SRow, Col),
		Path = [start(SRow, SCol)]
	;	% Could be a beam diverted by a splitter
		member(ColSideCalc, [Col - 1, Col + 1]),
		ColSide is ColSideCalc,
		splitter(SPRow, ColSide),
		SPRow > SRow,
		SPRow < Row,
		% Ensure there is no other splitter blocking the beam
		\+ splitter_blocking(SPRow, Row, Col),
		% Ensure there is a beam provided to the splitter
		SPRow0 is SPRow - 1,
		Path = [splitter(SPRow, ColSide, Col)|Path0],
		row_col_has_beam(SPRow0, ColSide, Path0)
	).

splitter_splits_beam(Row, Col, Direction) :-
	splitter(Row, Col),
	Row0 is Row - 1,
	once(row_col_has_beam(Row0, Col, _)),
	(	Col > 1,
		Direction = left
	;	count_cols(Cols),
		Col < Cols,
		Direction = right
	).

% Tabling is vital for performance (0.4 seconds)
:- table beam_splits_to_bottom/3.
beam_splits_to_bottom(Row, Col, Count) :-
	count_rows(EndRow),
	(	between(Row, EndRow, R),
		splitter(R, Col)
	->	Row1 is R + 1,
		Col0 is Col - 1,
		Col1 is Col + 1,
		(	Col0 > 0
		->	beam_splits_to_bottom(Row1, Col0, Left)
		;	Left = 0
		),
		(	count_cols(Cols),
			Col1 =< Cols
		->	beam_splits_to_bottom(Row1, Col1, Right)
		;	Right = 0
		),
		% Beams could split further
		Count is Left + Right
		% 1 beam escapes at the bottom
	;	Count = 1
	).

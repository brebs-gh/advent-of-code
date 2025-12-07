% Written in SWI-Prolog
% https://adventofcode.com/2025/day/6
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day6(P1, P2) :-
	once(phrase_from_file(homework(Ls), 'aoc2025_day6_input.txt')),
	writeln(parsed),
	Levels = [_|_],
	OpsL = [Ops],
	once(append([Levels, OpsL], Ls)),
	transpose_list(Levels, Nums),
	calc_nums(Nums, Ops, P1),
	cephalopod_nums(P2).

homework([Ns|T]) --> line_conv(Ns), homework_next(T).

homework_next([]) --> [].
homework_next([H|T]) --> homework([H|T]).

line_conv(Ns) -->
	length(spaces, SLen),
	line_of_words([H|T], SLen),
	{ maplist(conv, [H|T], Ns) }.

line_of_words([W|Ws], Col) -->
	word(W),
	spaces(E),
	{	length(W, WLen),
		length(E, ELen),
		Col1 is Col + WLen + ELen,
		!,
		assert_word(W, Col)
	},
	line_of_words_next(Ws, Col1).

line_of_words_next([], _) --> [10].
line_of_words_next([H|T], Col) --> line_of_words([H|T], Col).

% At least 1 char
word([H|T]) --> word_([H|T]).

word_(L), R --> [C], { bad_word_char(C), !, R = [C], L = [] }.
word_([H|T]) --> [H], word_(T).

bad_word_char(C) :- code_type(C, space).
bad_word_char --> [10].

% Example usage in DCG: length(spaces, SLen)
length(Pred, Len, L, T) :-
	call(Pred, Seq, L, T),
	length(Seq, Len).

spaces([32|T]) --> [32], !, spaces(T).
spaces([]) --> [].

assert_word([], _).
assert_word([H|T], Col) :-
	(	conv_op(H, Op)
	->	assertz(col_op(Col, Op))
	;	assertz(col_digit_code(Col, H)),
		Col1 is Col + 1,
		assert_word(T, Col1)
	).

transpose_list(Ns, T) :-
	findall(F, maplist(nth1(_), Ns, F), T).

conv(Cs, C) :-
	(	Cs = [Sing],
		conv_op(Sing, Op)
	->	C = Op
	;	number_codes(C, Cs)
	).

conv_op(0'+, plus_op).
conv_op(0'*, mult_op).

calc_nums([], [], 0).
calc_nums([[H|T]|NLs], [Op|Ops], Sum) :-
	foldl(Op, T, H, S),
	calc_nums(NLs, Ops, S0),
	Sum is S + S0.

plus_op(X, Y, P) :-
	P is X + Y.

mult_op(X, Y, M) :-
	M is X * Y.

col_op_next(Col, ColNext) :-
	% Find next
	(	col_op(ColN, _),
		ColN > Col
	->	ColNext = ColN
	;	aggregate_all(max(Col), col_number(Col, _), MaxCol),
		ColNext is MaxCol + 2
	).

cephalopod_nums(SumP2) :-
	setof(Col, Code^col_digit_code(Col, Code), DigitCols),
	% Find number per column
	forall(member(DigCol, DigitCols),
		(	findall(DigCode, col_digit_code(DigCol, DigCode), DigCodes),
			number_codes(ColAmount, DigCodes),
			assertz(col_number(DigCol, ColAmount))
		)
	),
	% Column numbers per Op
	forall(col_op(Col, Op),
		% Find the columns valid for this Op
		(	col_op_next(Col, ColNext),
			UpperCol is ColNext - 1,
			findall(ColAmount,
				(	between(Col, UpperCol, DigCol),
					col_number(DigCol, ColAmount)
				),
				OpAmounts
			),
			assertz(col_op_numbers(Col, Op, OpAmounts))
		)
	),
	% Perform calc for the numbers for each Op
	aggregate_all(sum(FoldSum),
		(	col_op_numbers(Col, Op, [H|T]),
			foldl(Op, T, H, FoldSum)
		),
		SumP2
	).

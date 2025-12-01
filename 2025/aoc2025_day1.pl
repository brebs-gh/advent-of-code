% https://adventofcode.com/2025/day/1

count_zeros(P1Z, P2Z) :-
	once(phrase_from_file(moves(s(50, P1ZS, P2ZS)), 'aoc2025_day1_part1_input.txt')),
	% Final calcuation of maths terms
	P1Z is P1ZS,
	P2Z is P2ZS.

% Unify ends of maths terms with 0
moves(s(_, 0, 0), [] , []).
moves(S) -->
	[D], str(Digits), [10], !,
	{ state_next(S, D, Digits, S1) },
	moves(S1).

str([]) --> [].
str([H|T]) --> [H], str(T).

% Code as data
direction_move(0'L, C, I, CN, C - I, CN < 0, 100 - I + C).
direction_move(0'R, C, I, CN, C + I, CN > 99, CN - 100).

direction_move_calc(D, C, I, C1, Add) :-
	direction_move(D, C, I, CN, Expr, Cond, OverflowExpr),
	CN is Expr,
	(	Cond
	->	C1 is OverflowExpr,
		crossed_zero_check(C, C1, Add)
	;	C1 = CN,
		Add = 0
	).

crossed_zero_check(C, C1, Add) :-
	(	% Must cross over 0, rather than start at 0
		C \== 0,
		% Ending on zero is checked later
		C1 \== 0
	->	Add = 1
	;	Add = 0
	).

state_next(s(C, Z, P2Z), D, Digits, s(C1, Z1, P2Z1)) :-
	number_codes(I, Digits),
	I100 is I mod 100,
	% Handle complete wraparound
	divmod(I, 100, Div100, I100),
	% The final sub-100 move
	direction_move_calc(D, C, I100, C1, Add),
	zero_inc(C1, AZ1),
	Z = AZ1 + Z1,
	zero_inc(C1, IncP2),
	% Is terms rather than a calculation
	P2Z = Div100 + Add + IncP2 + P2Z1.

zero_inc(C1, Inc) :-
	(	C1 is 0
	->	Inc = 1
	;	Inc = 0
	).

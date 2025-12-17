% Written in SWI-Prolog
% https://adventofcode.com/2025/day/11
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

% Takes 0.037 seconds

day11(P1, P2) :-
	once(phrase_from_file(reactor, 'aoc2025_day11_input.txt')),
	part1(P1),
	part2(P2).

reactor --> word_atom(Dev), ": ",
	seq_delim(word_atom, ` `, As),
	{	assertz(from_next_list(Dev, As)) },
	[10],
	reactor_next.

reactor_next --> ([] | reactor).

seq_delim(GoalName, Delim, [H|T]) --> call(GoalName, H),
	( Delim, !, seq_delim(GoalName, Delim, T) | [], { T = [] } ).

% At least 1 char
word_atom(A) --> word_codes([H|T]), { atom_codes(A, [H|T]) }.

word_codes(L), R --> [C], { bad_word_code(C), !, R = [C], L = [] }.
word_codes([H|T]) --> [H], word_codes(T).

% Includes newline codes 10 and 13
bad_word_code(C) :- code_type(C, space).
bad_word_code(0',).
bad_word_code(0':).

part1(P1) :-
	% Count the paths
	paths_from_to_count(you, out, avoid([]), P1).

% With inspiration from
% https://www.reddit.com/r/adventofcode/comments/1pmuayw/2025_day_11_part_2_was_i_the_only_one_who_used/
part2(P2) :-
	% To dac or fft, as the first step
	paths_from_to_count(svr, dac, avoid([fft, out]), SD),
	paths_from_to_count(svr, fft, avoid([dac, out]), SF),
	% Between dac and fft
	paths_from_to_count(dac, fft, avoid([svr, out]), DF),
	paths_from_to_count(fft, dac, avoid([svr, out]), FD),
	% To out
	paths_from_to_count(dac, out, avoid([svr, fft]), DO),
	paths_from_to_count(fft, out, avoid([svr, dac]), FO),
	% Via dac then fft, or via fft then dac
	% Is an enormous integer
	P2 is (SD * DF * FO) + (SF * FD * DO).

% Tabling is essential for performance
:- table paths_from_to_count/4.
paths_from_to_count(From, To, avoid(Avoids), Count) :-
	from_next_list(From, Nexts),
	aggregate_all(sum(C),
		(	member(Next, Nexts),
			\+ memberchk(Next, Avoids),
			(	Next = To
			->	% Arrived at destination
				C = 1
			;	% Search recursively
				% The table prevents infinite loop
				paths_from_to_count(Next, To, avoid(Avoids), C)
			)
		), Count
	).

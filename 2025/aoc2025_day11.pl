% Written in SWI-Prolog
% https://adventofcode.com/2025/day/11
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day11(P1, P2) :-
	once(phrase_from_file(reactor, 'aoc2025_day11_input.txt')),
	part1(P1).

reactor --> word(Dev), ": ",
	seq_delim(word, ` `, As),
	{	forall(member(A, As), assertz(from_next(Dev, A))) },
	[10],
	reactor_next.

reactor_next --> ([] | reactor).

seq_delim(GoalName, Delim, [H|T]) --> call(GoalName, H),
	( Delim, !, seq_delim(GoalName, Delim, T) | [], { T = [] } ).

% At least 1 char
word(A) --> word_([H|T]), { atom_codes(A, [H|T]) }.

word_(L), R --> [C], { bad_word_char(C), !, R = [C], L = [] }.
word_([H|T]) --> [H], word_(T).

% Includes newline codes 10 and 13
bad_word_char(C) :- code_type(C, space).
bad_word_char(0',).
bad_word_char(0':).

part1(P1) :-
	% Count the paths
	aggregate_all(count, path(you), P1).

path(From) :-
	from_next(From, out).
path(From) :-
	from_next(From, Next),
	path(Next).

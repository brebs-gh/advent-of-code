% Written in SWI-Prolog
% https://adventofcode.com/2025/day/10
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

% Runs in 2 seconds

day10(P1, P2) :-
	once(phrase_from_file(machines(Ms), 'aoc2025_day10_input.txt')),
	writeln(parsed),
	part1(Ms, P1).
	%part2(P2).

machines([]) --> end_of_input.
machines([M|Ms]) --> machine(M), [10], machines(Ms).

machine(m(sc(SC), b(Bs), j(Js))) --> startup_config(SC), " ", buttons(Bs), joltages(Js).

startup_config(Ts) --> "[", hash_toggles(Ts), "]".

hash_toggles([]) --> [].
hash_toggles([H|T]) --> hash_toggle(H), hash_toggles(T).

% 0 and 1 is easier to visually distinguish than on vs off
hash_toggle(t(0)) --> ".".
hash_toggle(t(1)) --> "#".

buttons([Is|T]) --> "(", integers(Is), ") ", buttons_next(T).

buttons_next([]) --> [].
buttons_next([H|T]) --> buttons([H|T]).

joltages(Js) --> "{", integers(Js), "}".

integers([I|Is]) --> integer(I), integers_next(Is).

integers_next([]) --> [].
integers_next([H|T]) --> ",", integers([H|T]).

integer(I) --> digits([H|T]), { number_codes(I, [H|T]) }.

digits([D|Ds]) --> [D], { between(0'0, 0'9, D) }, digits(Ds).
digits([]) --> [].

end_of_input([], []).

toggle_next(0, 1).
toggle_next(1, 0).

part1(Ms, P1) :-
	maplist(machine_fewest_button_presses, Ms, Fewest),
	!,
	sum_list(Fewest, P1).

machine_fewest_button_presses(Machine, PressCount) :-
	Machine = m(sc(SCs), b(Bs), _),
	same_length(SCs, Cs),
	% Start in off positions
	maplist(initial_state_toggle_off, Cs),
	length(Presses, PressCount),
	machine_button_presses(Presses, Bs, SCs, Cs).

initial_state_toggle_off(t(0)).

machine_button_presses(L, _Bs, SCs, Cs) :-
	SCs = Cs,
	!,
	L = [].
machine_button_presses([_|T], Bs, SCs, Cs) :-
	% Choose a button to press
	member(BIs, Bs),
	% Updates Cs using setarg
	press_buttons(BIs, Cs),
	machine_button_presses(T, Bs, SCs, Cs).

press_buttons([], _).
press_buttons([BI|BIs], Cs) :-
	nth0(BI, Cs, TC),
	arg(1, TC, C),
	toggle_next(C, Next),
	setarg(1, TC, Next),
	press_buttons(BIs, Cs).

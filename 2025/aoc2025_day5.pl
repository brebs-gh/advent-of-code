% Written in SWI-Prolog
% https://adventofcode.com/2025/day/5
% https://swi-prolog.discourse.group/t/advent-of-code-2025/9406

day5(P1, P2) :-
	once(phrase_from_file(ingredients, 'aoc2025_day5_input.txt')),
	% setof conveniently sorts the list
	setof(Id, id(Id), Ids),
	setof(R, range(R), Rs),
	merge_overlapping_ranges(Rs, RsM),
	fresh_ings(Ids, RsM, FreshIds),
	!,
	length(FreshIds, P1),
	count_range_sizes(RsM, P2).

ingredients --> ranges, [10], ingredient_ids.

ranges -->
	digs(S), "-", digs(E), [10],
	{	number_codes(L, S),
		number_codes(U, E),
		assertz(range(L-U))
	},
	ranges_next.

ranges_next --> [].
ranges_next --> ranges.

ingredient_ids --> digs(Ds), [10],
	{	number_codes(I, Ds),
		assertz(id(I))
	},
	ingredient_ids_next.

ingredient_ids_next --> [].
ingredient_ids_next --> ingredient_ids.

% At least 1 digit
digs([H|T]) --> digs_([H|T]).

digs_([H|T]) --> dig(H), digs_(T).
digs_([]) --> [].

dig(D) --> [D], { between(0'0, 0'9, D) }.

merge_overlapping_ranges([], []).
merge_overlapping_ranges([R], [R]).
merge_overlapping_ranges([L1-U1, L2-U2|Rs], RsM) :-
	(	(	L2 >= L1, U1 >= L2
		;	L1 >= L2, U2 >= L1
		)
	->	% Overlapping - merge the 2 rows
		Max is max(U1, U2),
		Min is min(L1, L2),
		merge_overlapping_ranges([Min-Max|Rs], RsM)
	;	L2 > U1,
		RsM = [L1-U1|RsM0],
		merge_overlapping_ranges([L2-U2|Rs], RsM0)
	).

fresh_ings([], _, []).
% If there are no more fresh ranges, then no more fresh Ids
fresh_ings([_|_], [], []).
fresh_ings([Id|Ids], [L-U|Rs], FreshIds) :-
	(	Id > U
	->	% Can stop checking this range, it is too low
		fresh_ings([Id|Ids], Rs, FreshIds)
	;	Id < L
	->	% Id is not fresh
		fresh_ings(Ids, [L-U|Rs], FreshIds)
	;	FreshIds = [Id|FreshIds0],
		fresh_ings(Ids, [L-U|Rs], FreshIds0)
	).

count_range_sizes(Rs, P2) :-
	aggregate_all(sum(D), (member(L-U, Rs), D is U - L + 1), P2).

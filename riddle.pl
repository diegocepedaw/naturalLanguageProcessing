% Prog Lang PA3

%%  office(-Solution)
%   @param  Solution is a list of offices that satisfy all constraints.
%   @author Folklore attributes this puzzle to Einstein
%   @see http://en.wikipedia.org/wiki/taekwondo_Puzzle
% Render the offices term as a nice table.

% tmatch(Owner) :-
% 	write('taekwoncheck: '),
% 	office(Hs),
% 	all_distinct(Hs),
% 	match(h(Owner,taekwondo,_,_,_), Hs).

% Single Attributes
match(A, list(A, _, _, _, _)).
match(A, list(_, A, _, _, _)).
match(A, list(_, _, A, _, _)).
match(A, list(_, _, _, A, _)).
match(A, list(_, _, _, _, A)).

% Static Positions
first(A, list(A, _, _, _, _)).
second(A, list(_, A, _, _, _)).
third(A, list(_, _, A, _, _)).
fourth(A, list(_, _, _, A, _)).
fifth(A, list(_, _, _, _, A)).

% Relative Positions
leftOf(L,R, list(L,R,_,_,_)).
leftOf(L,R, list(_,L,R,_,_)).
leftOf(L,R, list(_,_,L,R,_)).
leftOf(L,R, list(_,_,_,L,R)).

nextTo(A, B, list(B, A, _, _, _)).
nextTo(A, B, list(_, B, A, _, _)).
nextTo(A, B, list(_, _, B, A, _)).
nextTo(A, B, list(_, _, _, B, A)).
nextTo(A, B, list(A, B, _, _, _)).
nextTo(A, B, list(_, A, B, _, _)).
nextTo(A, B, list(_, _, A, B, _)).
nextTo(A, B, list(_, _, _, A, B)).

% Taekwondo Major Solve
tmember(Owner) :-
	write('Major in Taekwondo\n '),
	office(Hs),
	match(h(Owner,taekwondo,_,_,_), Hs).

% List of Rules
office(Hs) :-
	% each office in the list Hs of offices is represented as:
	%      h(Major, Club, Pizza, Book, Poster)
	match(h(architecture,_,_,_,cad), Hs),					%  1
	match(h(cse,flying,_,_,_), Hs),						%  2
	leftOf(h(_,_,_,_,dilbert), h(_,_,_,_,calvin), Hs),		%  4
	match(h(gsas,_,_,scifi,_), Hs), 						%  3
	match(h(_,_,_,fantasy,dilbert), Hs),					%  5
	match(h(_,rcos,pepperoni,_,_), Hs),					%  6
	match(h(_,_,cheese,_,xkcd), Hs),						%  7
	third(h(_,_,_,fiction,_), Hs),							%  8
	first(h(cs,_,_,_,_), Hs),								%  9
	nextTo(h(_,_,buffalo,_,_), h(_,rgaming,_,_,_), Hs),		% 10
	nextTo(h(_,_,cheese,_,_), h(_,compsci,_,_,_),  Hs),		% 11
	match(h(_,_,hawaiian,poetry,_), Hs),					% 12
	match(h(itws,_,broccoli,_,_), Hs),						% 13
	nextTo(h(cs,_,_,_,_), h(_,_,_,_,phd), Hs),				% 14
	nextTo(h(_,_,buffalo,_,_), h(_,_,_,history,_), Hs), 	% 15
	match(h(_,taekwondo,_,_,_), Hs).

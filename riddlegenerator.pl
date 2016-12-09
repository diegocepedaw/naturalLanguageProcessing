% by Peter Ko & Diego Cepeda
% Prog Lang PA3
% Part 2


s --> np, vp.                /* sentence */

np --> pd, n, rel. 			 /* noun phrase */
np --> pd, n.
np --> pd, vp.
np --> pd, n, np.
np --> pd, np.
np --> pd, np, n.
np --> pd, pn, n.
np --> pd, pn, np.
np --> pn, n, vp.
np --> pn, n.
np --> pn.
np --> n, n, n.
np --> n, n, rel.
np --> n, n.
np --> n.

vp --> tv, np.              /* verb phrase */

rel --> [].                 /*  relative clause */
rel --> rpn, vp.

pn --> major.				/* uniques */
pn --> book.
pn --> comic.
pn --> club.
pn --> pizza.


major --> [MAJOR], {major(MAJOR)}.
major("architecture").
major("cs").
major("cse").
major("gs").
major("itws").


club --> [CLUB], {club(CLUB)}.
club("rpi_flying").
club("rcos").
club("compsci").
club("r_gaming_alliance").
club("taekwondo").


pizza --> [PIZZA], {pizza(PIZZA)}.
pizza("buffalo_chicken").
pizza("broccoli").
pizza("hawaiian").
pizza("pepperoni").
pizza("cheese").


book --> [BOOK], {book(BOOK)}.
book("history").
book("poetry").
book("sci_fi").
book("fiction").
book("fantasy").


comic --> [COMIC], {comic(COMIC)}.
comic("phd_comics").
comic("xkcd").
comic("dilbert").
comic("ctrl_alt_del").
comic("calvin_and_hobbes").


rpn --> [RPN], {rpn(RPN)}.   /* relative pronoun */
rpn("that").
rpn("which").
rpn("who").

pd --> d.
pd --> d, p.
pd --> p, d.
pd --> p, p, d.
pd --> p, d, p.
pd --> p, d, p, p, d.

p --> pp.
p --> pa.

pp --> [PREP], {pp(PREP)}.
pp("on").
pp("with").
pp("in").
pp("to").
pp("of").

pa --> pa1.
pa --> pa2.

pa1 --> [PREP], {pa1(PREP)}.
pa1("next").
pa1("left").
pa1("neighbor").

pa2 --> [PREP], {pa2(PREP)}.
pa2("middle").
pa2("first").

d --> [DET], {d(DET)}.       /* determiner */
d("the").
d("an").


n --> [N], {n(N)}.           /* noun */
n("major").
n("office").
n("comic").
n("poster").
n("club").
n("occupant").
n("student").
n("pizza").
n("one").
n("neighbor").


tv --> [TV], {tv(TV)}.       /* transitive verb */
tv("reads").
tv("occupies").
tv("occupying").
tv("belongs").
tv("eats").
tv("has").
tv("is").


% I/O adapted from
% http://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

generateRiddle(File) :-
    open('rules.txt', write, StrApo),
    write(StrApo, "'office(Hs) :-"),
    close(StrApo),

    open(File, read, Str),
    read_file(Str,Lines),
    %write(Lines),
    split(Lines),
    close(Str).

executeRiddle() :-
    open('rules.txt', append, StrPrd),
    write(StrPrd, "match(h(_,taekwondo,_,_,_), Hs).'."),
    close(StrPrd),

    open('rules.txt', read, Str2),
    read_file(Str2,RuleBody),
    [H|_] = RuleBody,
    %write(H),
    close(Str2),

    term_string(A, H),
    %write(A), nl,
    assert(A).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

split([]) :- !.
split([H|T]) :- split2(H), split(T).

split2([]).
split2(end_of_file) :- !.
split2(Line) :-
  string_lower(Line, LowerLine),
  split_string(LowerLine, " ", "+", Hs),
  %write(Hs), nl,
  s(Hs, []) ->
    %write('Sentence is true'), nl,
    solver(Hs, ["_","_","_","_","_"], ["_","_","_","_","_"], 0, "match");
  executeRiddle().

solver([], OL, OL2, Flag, Trigger) :-
  %write(OL), nl,
  %write(OL2) ,nl,
  %write(Flag), nl,
  %write(Trigger), nl,
  cluetofile(OL, OL2, Flag, Trigger).

solver([H|T],OL, OL2, Flag, Trigger) :-
  ( major(H) ->
      %write('Is a major'), nl,
      (
        Flag =:= 0 -> (
          replace(OL, 0, H, OLTemp),
          solver(T, OLTemp, OL2, Flag, Trigger)
        );
        Flag =\= 0 -> (
          replace(OL2, 0, H, OLTemp),
          solver(T, OL, OLTemp, Flag, Trigger))
      )
    , !;
    club(H) ->
      %write('Is a club'), nl,
      (
        Flag =:= 0 -> (
          replace(OL, 1, H, OLTemp),
          solver(T, OLTemp, OL2, Flag, Trigger)
        );
        Flag =\= 0 -> (
          replace(OL2, 1, H, OLTemp),
          solver(T, OL, OLTemp, Flag, Trigger))
      )
    , !;
    pizza(H) ->
      %write('Is a pizza'), nl,
      (
        Flag =:= 0 -> (
          replace(OL, 2, H, OLTemp),
          solver(T, OLTemp, OL2, Flag, Trigger)
        );
        Flag =\= 0 -> (
          replace(OL2, 2, H, OLTemp),
          solver(T, OL, OLTemp, Flag, Trigger))
      )
    , !;
    book(H) ->
      %write('Is a book'), nl,
      (
        Flag =:= 0 -> (
          replace(OL, 3, H, OLTemp),
          solver(T, OLTemp, OL2, Flag, Trigger)
        );
        Flag =\= 0 -> (
          replace(OL2, 3, H, OLTemp),
          solver(T, OL, OLTemp, Flag, Trigger))
      )
    , !;
    comic(H) ->
      %write('Is a comic'), nl,
      (
        Flag =:= 0 -> (
          replace(OL, 4, H, OLTemp),
          solver(T, OLTemp, OL2, Flag, Trigger)
        );
        Flag =\= 0 -> (
          replace(OL2, 4, H, OLTemp),
          solver(T, OL, OLTemp, Flag, Trigger))
      )
    , !;
    pa1(H) -> (
      %write('RELATIVE POSITION FLAG'), nl,
      solver(T, OL, OL2, 1, H) ), !
    ;
    pa2(H) -> (
      %write('ABSOLUTE POSITION FLAG'), nl,
      solver(T, OL, OL2, Flag, H) ), !
    ;
    d(H) -> (
      %write('Is a word'), nl,
      solver(T, OL, OL2, Flag, Trigger) ), !
    ;
    rpn(H) -> (
      %write('Is a word'), nl,
      solver(T, OL, OL2, Flag, Trigger) ), !
    ;
    pp(H) -> (
      %write('Is a word'), nl,
      solver(T, OL, OL2, Flag, Trigger) ), !
    ;
    n(H) -> (
      %write('Is a word'), nl,
      solver(T, OL, OL2, Flag, Trigger) ), !
    ;
    tv(H) -> (
      %write('Is a verb'), nl,
      solver(T, OL, OL2, Flag, Trigger) ), !
  ), !.

% Replace function from:
% http://stackoverflow.com/questions/8519203/prolog-replace-an-element-in-a-list-at-a-specified-index
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

printlist(_, []) :- !.
printlist(Stream, [H|T]) :-
  write(Stream,H),
  checkcomma(Stream, T),
  printlist(Stream, T).

checkcomma(_, []) :- !.
checkcomma(Stream, _) :-
  write(Stream,",").

cluetofile(OL, _, 0, "match") :-
  open('rules.txt', append, Stream),
  write(Stream,"match(h("),
  printlist(Stream, OL),
  write(Stream,"), Hs),"),
  close(Stream).

cluetofile(OL, _, 0, "first") :-
  open('rules.txt', append, Stream),
  write(Stream,"first(h("),
  printlist(Stream, OL),
  write(Stream,"), Hs),"),
  close(Stream).

cluetofile(OL, _, 0, "middle") :-
  open('rules.txt', append, Stream),
  write(Stream,"third(h("),
  printlist(Stream, OL),
  write(Stream,"), Hs),"),
  close(Stream).

cluetofile(OL, OL2, 1, "next") :-
  open('rules.txt', append, Stream),
  write(Stream,"next(h("),
  printlist(Stream, OL), write(Stream,"), h("), printlist(Stream, OL2),
  write(Stream,"), Hs),"),
  close(Stream).

cluetofile(OL, OL2, 1, "left") :-
  open('rules.txt', append, Stream),
  write(Stream,"left(h("),
  printlist(Stream, OL), write(Stream,"), h("), printlist(Stream, OL2),
  write(Stream,"), Hs),"),
  close(Stream).

cluetofile(OL, OL2, 1, "neighbor") :-
  open('rules.txt', append, Stream),
  write(Stream,"next(h("),
  printlist(Stream, OL), write(Stream,"), h("), printlist(Stream, OL2),
  write(Stream,"), Hs),"),
  close(Stream).

% Part 1 rules

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
left(L,R, list(L,R,_,_,_)).
left(L,R, list(_,L,R,_,_)).
left(L,R, list(_,_,L,R,_)).
left(L,R, list(_,_,_,L,R)).

next(A, B, list(B, A, _, _, _)).
next(A, B, list(_, B, A, _, _)).
next(A, B, list(_, _, B, A, _)).
next(A, B, list(_, _, _, B, A)).
next(A, B, list(A, B, _, _, _)).
next(A, B, list(_, A, B, _, _)).
next(A, B, list(_, _, A, B, _)).
next(A, B, list(_, _, _, A, B)).

% Taekwondo Major Solve
tmember(Owner) :-
	write('Major in Taekwondo\n '),
	office(Hs),
	match(h(Owner,taekwondo,_,_,_), Hs).

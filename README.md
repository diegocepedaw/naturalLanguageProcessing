"Programming Languages Fall 2016 PA3" Diego Cepeda -- Peter Ko

References:
http://www.dtic.mil/dtic/tr/fulltext/u2/a205268.pdf
http://www.swi-prolog.org/
swish.swi-prolog.org

Outside code used:
Prolog Zebra Puzzle Syntax: https://gist.github.com/ex/68d27c3acef502457073
List to String: http://stackoverflow.com/questions/5879956/prolog-list-to-string
Replace Element in List: http://stackoverflow.com/questions/8519203/prolog-replace-an-element-in-a-list-at-a-specified-index

Commands to run Part 1:
consult(riddle).
tmember(T).

Commands to run Part 2:
consult(riddlegenerator).
generateRiddle("clues.txt").
tmember(T).

Changes to input file:
We underscored names i.e r gaming alliance -> r_gaming_aliance
Used unique names i.e cs club renamed to CompSci club
added single quotes and periods to input file

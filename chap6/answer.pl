/* Requires satisfy-pc.pl */

answer(Sit,X^For,Ans) :-
	var(X),!,
	domain(Sit,Dom),
	setof(X,(member(X,Dom),satisfy(Sit,For)),Ans).
answer(Sit,yes^For,yes) :-
	satisfy(Sit,For),!.
answer(Sit,yes^For,no).
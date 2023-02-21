/* This is a version of Program 4.5 on p. 124 of Pereira and Shieber.
It differs from the original program in that it employs the method of gap
threading discussed in the following section (4.2.7). It also uses the lambda
calculus in the manner of eng-lambda.pl.  I have also added some 
extra rules and fixed a bug.  rhc */

/* Needs beta.pl and logic-lexicon.pl */


% Operators for connectives

:- op(500,xfy,[&,\/]).
:- op(550,xfy,--->).
:- op(450,fx,~).

% Operator for functional application
:- op(400, yfx, *).

:- [beta,'logic-lexicon'].

q(VP) --> whpron, vp(VP, []-[]).
q(X^S) --> whpron, sinv(S, [gap(np,X)]-[]).
q(yes^S) --> sinv(S,[]-[]).

s(S) --> s(S,[]-[]).
s(NP*VP,In-Out) --> np(NP, In-In), vp(VP, In-Out).

sinv(NP*VP,In-Out) --> aux, np(NP, In-In), vp(VP, In-Out).

sbar(S,In-Out) --> [that], s(S,In-Out).

np(Det*(Rel*N), In-In) --> det(Det), n(N), optrel(Rel).
np(P^(P*X), In-In) --> pn(X).
np(P^(P*X), [gap(np,X)|Out]-Out) --> [].

vp(X^(NP*(Y^(TV*Y*X))), In-Out) --> tv(TV), np(NP, In-Out).
vp(VP, In-In) --> iv(VP).
vp(STV*Sbar, In-Out) --> stv(STV), sbar(Sbar,In-Out).

optrel(N^N) --> [].
optrel(P^(X^(P*X&VP*X))) --> relpron, vp(VP, []-[]).
optrel(P^(X^(P*X&S))) --> relpron, s(S, [gap(np, X)|Out]-Out).

det(LF) --> [D], {det(D,LF)}.
det(every, Q^P^every(X,Q*X ---> P*X) ).
det(a, Q^P^some(X,Q*X & P*X) ).

n(LF) --> [N], {n(N,LF)}.
n(program, X^program(X) ).
n(student, X^student(X) ).

pn(E) --> [PN], {pn(PN, E)}.
pn(terry, terry).
pn(shrdlu, shrdlu).

tv(LF) --> [TV], {tv(TV,LF)}.
tv(wrote, Y^X^write(X,Y) ).
tv(write, Y^X^write(X,Y) ).
tv(writing, Y^X^write(X,Y) ).

stv(LF) --> [STV], {stv(STV,LF)}.
stv(said, Y^X^say(X,Y) ).
stv(say, Y^X^say(X,Y) ).
stv(saying, Y^X^say(X,Y) ).

iv(LF) --> [IV], {iv(IV,LF)}.
iv(halts, X^halt(X) ).

relpron --> [RelPron], {relpron(RelPron)}.
relpron(that).
relpron(who).
relpron(whom).

whpron --> [WhPron], {whpron(WhPron)}.
whpron(what).
whpron(who).
whpron(whom).

aux --> [Aux], {aux(Aux)}.
aux(is).
aux(did).

parse(S,LF) :-
	(q(L,S,[]);
	s(L,S,[])),
	convert_question(L,LF).
	
convert_question(X^P,X^Q) :-
	!,convert(P,Q).
convert_question(P,Q) :-
	convert(P,Q).

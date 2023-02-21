% Operators for connectives

:- op(500,xfy,[&,\/]).
:- op(550,xfy,--->).
:- op(450,fx,~).

% Operator for functional application
:- op(400, yfx, *).


% Definition of well-formed formulas

wff(For) :-
	var(For),!.
wff(For) :- 
	For =..[P|Args],
	predicate(P,Args),
	terms(Args).
wff((For1 & For2)) :-
	wff(For1),
	wff(For2).
wff((For1 \/ For2)) :-
	wff(For1),
	wff(For2).
wff((For1 ---> For2)) :-
	wff(For1),
	wff(For2).  
wff((~ For)) :-
	wff(For).
wff((Lambda_term*Arg)) :-
	lambda_term(Lambda_term,s(0)),
	term(Arg).

wff(For) :-
	functor(For,Q,2),
	quantifier(Q),
	For =.. [Q,V,For1],
	var(V),
	wff(For1).
	

% Terms are constants or variables
		 
terms([]).
terms([X|L]) :-
	term(X),
	terms(L).
	
term(X) :- var(X),!.
term(X) :- constant(X).
term(X) :- wff(X).
term(X) :- lambda_term(X,_).

% Lambda terms.
lambda_term(P,_) :-
	var(P),!.
lambda_term(X^For,s(0)) :-
	var(X), 
	wff(For).
lambda_term(X^Lambda_term,s(N)) :-
	var(X),
	lambda_term(Lambda_term,N).
lambda_term(Lambda_term*Arg,s(N)) :-
	lambda_term(Lambda_term,s(s(N))),
	term(Arg).

% A sample lexicon.  Arity n represented by a list of length n.

predicate(run,[_]).
predicate(man,[_]).
predicate(woman,[_]).

predicate(love,[_,_]).

constant(a).
constant(b).
constant(c).




/* Quantifiers do not properly belong in the lexicon but we will want
to add more quantifiers as we extend predicate calculus */

quantifier(every).
quantifier(some).


% Ancillary syntactic notion
	
closed_atomic_formula(For) :-
	For =.. [_|Args],
	constants(Args).
	
constants([]).
constants([Arg|Args]) :-
	\+ var(Arg),
	constant(Arg),
	constants(Args).

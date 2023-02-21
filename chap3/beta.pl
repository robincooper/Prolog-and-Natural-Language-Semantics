% beta.pl

/* Requires a lexicon for the lambda calculus */ 

% Operators for connectives

:- op(500,xfy,[&,\/]).
:- op(550,xfy,--->).
:- op(450,fx,~).

% Operator for functional application
:- op(400, yfx, *).
   

convert(P,P) :-
	var(P),!.
convert(P*A,P*A) :-
	var(P),!.
convert(X^P*A,Q) :- !,
	reduce(X^P*A,P1),
	convert(P1,Q).
convert(P*A,Q) :- 
	convert(P,P1),
	\+ P = P1,!,    % Prevent left recursion when P and P1 are identical
	convert(P1*A,Q).
convert(P&Q,P1&Q1) :- !,
	convert(P,P1),
	convert(Q,Q1).
convert(P\/Q,P1\/Q1) :- !,
	convert(P,P1),
	convert(Q,Q1).
convert(P--->Q,P1--->Q1) :- !,
	convert(P,P1),
	convert(Q,Q1).
convert(~P,~P1) :- !,
	convert(P,P1).
convert(P,Q) :-
	P =..[Quant,Var,Scope],
	quantifier(Quant),!,
	convert(Scope,NewScope),
	Q =..[Quant,Var,NewScope].
convert(P,Q) :-
	P =..[Pred|Args],
	predicate(Pred,Args),!,
	convert_all(Args,NewArgs),
	Q =..[Pred|NewArgs].
convert(P,P).

convert_all([],[]).
convert_all([P|L],[Q|L1]) :-
	convert(P,Q),
	convert_all(L,L1).

reduce(X^P*X,P).


	






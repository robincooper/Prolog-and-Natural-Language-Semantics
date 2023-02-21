
:- op(500,xfy,[&,\/]).
:- op(550,xfy,--->).
:- op(450,fx,~).

:- dynamic domain/2.

satisfy(Sit,P) :- 
 	% closed_atomic_formula(P),
	support(Sit,P).
	
satisfy(Sit, P&Q) :-
	satisfy(Sit,P),
	satisfy(Sit,Q).
	
satisfy(Sit, P\/Q) :-
	(satisfy(Sit,P); satisfy(Sit,Q)).
	
satisfy(Sit, P--->Q) :-
	(satisfy(Sit,~P); satisfy(Sit,Q)).
	
satisfy(Sit, ~P) :-
	\+ satisfy(Sit,P).
	
satisfy(Sit,some(X,P)) :-
	domain(Sit,Dom),
	\+ \+ (member(X,Dom),satisfy(Sit,P)).
	
satisfy(Sit,some(X,P,Q)) :-
	setof(X,(satisfy(Sit,P),satisfy(Sit,Q)),_).

satisfy(Sit,no(X,P,Q)) :-
	\+ setof(X,(satisfy(Sit,P),satisfy(Sit,Q)),_).
satisfy(Sit,one(X,P,Q)) :-
	 setof(X,(satisfy(Sit,P),satisfy(Sit,Q)),[_]).
satisfy(Sit,two(X,P,Q)) :-
	 setof(X,(satisfy(Sit,P),satisfy(Sit,Q)),[_,_]).
satisfy(Sit,exactly(Num,X,P,Q)) :-
	 setof(X,(satisfy(Sit,P),satisfy(Sit,Q)),Set),
	 length(Set,Num).
satisfy(Sit,at_least(Num,X,P,Q)) :-
	 set_of(X,(satisfy(Sit,P),satisfy(Sit,Q)),Set),
	 length(Set,N),
	 N >= Num.
satisfy(Sit,at_most(Num,X,P,Q)) :-
	 set_of(X,(satisfy(Sit,P),satisfy(Sit,Q)),Set),
	 length(Set,N),
	 N =< Num.
	
satisfy(Sit,every(X,P)) :-
	domain(Sit,Dom),
	all(Dom,X,satisfy(Sit,P)).
	
satisfy(Sit,every(X,P,Q)) :-
	 set_of(X,(satisfy(Sit,P),satisfy(Sit,Q)),Set),
	 set_of(X,satisfy(Sit,P),Set).
	 
satisfy(Sit,many(Num,X,P,Q)) :-
	 set_of(X,(satisfy(Sit,P),satisfy(Sit,Q)),Set),
	 length(Set,N),
	 N >= Num.
satisfy(Sit,few(Num,X,P,Q)) :-
	 set_of(X,(satisfy(Sit,P),satisfy(Sit,Q)),Set),
	 length(Set,N),
	 N =< Num.
satisfy(Sit,most(Prop,X,P,Q)) :-
	 set_of(X,(satisfy(Sit,P),satisfy(Sit,Q)),Set),
	 set_of(X,satisfy(Sit,P),Range),
	 length(Set,N),
	 length(Range,NR),
	 N/NR >= Prop.
	 
domain(Sit,Dom) :-
	setof(X,
		P^Fact^Args^(support(Sit,Fact),
			Fact =..[P|Args],member(X,Args)),Dom),
	asserta((domain(Sit,Dom) :- !)).
	
support(Sit,Fact) :-
	Clause =.. [Sit,Fact],
	Clause.


all([],_,_).
all([X|Dom],Y,Clause) :-
	\+ \+ (X = Y,Clause),
	all(Dom,Y,Clause).	
	
set_of(X,Goal,Set) :-
	setof(X,Goal,L),!,
	L = Set.
set_of(_,_,[]).

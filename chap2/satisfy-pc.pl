% Operators for connectives

:- op(500,xfy,[&,\/]).
:- op(550,xfy,--->).
:- op(450,fx,~).
:- dynamic domain/2.


% Satisfaction

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
        exists(X,Dom,satisfy(Sit,P)).
        
satisfy(Sit,every(X,P)) :-
        domain(Sit,Dom),
        all(X,Dom,satisfy(Sit,P)).
        
satisfy(Sit,P) :-
        fact(P), 
        support(Sit,P).

% Representation of situations
        
support(Sit,Fact) :-
        Clause =.. [Sit,Fact],
        Clause.
        
fact(Fact) :-
        Fact =.. [_|Args],
        atoms(Args).
        
atoms([]).
atoms([X|L]) :-
        atom(X),
        atoms(L).

% Quantifying over domains

domain(Sit,Dom) :-
        setof(X,
              Fact^P^Args^(support(Sit,Fact),
              Fact =..[P|Args],member(X,Args)),Dom),
        asserta((domain(Sit,Dom) :- !)).
        
all(_,[],_).
all(X,[Y|Dom],Clause) :-
        \+ \+ (X = Y,Clause),
        all(X,Dom,Clause).        

exists(X,Dom,Clause) :-
        \+ \+ (member(X,Dom),Clause).
        
% Utility
reset_domain(Sit) :-
        retract((domain(Sit,_) :- !)).
        

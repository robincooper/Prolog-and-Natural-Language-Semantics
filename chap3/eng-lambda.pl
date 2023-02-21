                     %Grammar with semantic representation
%An incomplete modification of eng-pc.pl using of the lambda-calculus
                                

% Operators for connectives

:- op(500,xfy,[&,\/]).
:- op(550,xfy,--->).
:- op(450,fx,~).

% Operator for functional application
:- op(400, yfx, *).

sentence(NP*VP) --> noun_phrase(NP), verb_phrase(VP).

noun_phrase(Det*Noun) -->
    determiner(Det), noun(Noun).

noun_phrase(P^(P*X)) --> proper_noun(X).

verb_phrase(X^(NP*(Y^(TV*Y*X)))) -->
    trans_verb(TV), noun_phrase(NP).
verb_phrase(IV) --> intrans_verb(IV).



determiner(Q^P^every(X,Q*X ---> P*X)) --> [every].
determiner(Q^P^some(X,Q*X & P*X)) --> [a].

noun(X^man(X)) --> [man].
noun(X^woman(X)) --> [woman].

proper_noun(john) --> [john].
proper_noun(mary) --> [mary].

trans_verb(Y^X^love(X,Y)) --> [loves].

intrans_verb(X^live(X)) --> [lives].

interpret(S, Logic) :- sentence(Logic, S,[]).

                     %Grammar with semantic representation
                               %Fernando Pereira
                                %Modified by RC

:- op(500,xfy,[&,\/]).
:- op(550,xfy,--->).
:- op(450,fx,~).

sentence(For) --> noun_phrase(X,Scope,For), verb_phrase(X,Scope).

noun_phrase(X,Scope,For) -->
    determiner(X,Range,Scope,For), noun(X,Range_noun),
    rel_clause(X,Range_noun,Range).
noun_phrase(X, Scope, For) -->
    determiner(X, Range, Scope, For), noun(X, Range).
noun_phrase(X,Scope,Scope&For_rel) -->
    proper_noun(X), non_restr_rel_clause(X, Scope, Scope&For_rel).
noun_phrase(X,For,For) --> proper_noun(X).

verb_phrase(X,For) -->
    trans_verb(X,Y,For_verb), noun_phrase(Y,For_verb,For).
verb_phrase(X,For) --> intrans_verb(X,For).

rel_clause(X,For,For&For_rel) --> [that], verb_phrase(X,For_rel).
non_restr_rel_clause(X,Scope,Scope&For_rel) -->
        [who], verb_phrase(X,For_rel).

determiner(X,Range,Scope,every(X,Range ---> Scope)) --> [every].
determiner(X,Range,Scope,some(X,Range & Scope)) --> [a].

noun(X,man(X)) --> [man].
noun(X,woman(X)) --> [woman].

proper_noun(john) --> [john].
proper_noun(mary) --> [mary].

trans_verb(X,Y,love(X,Y)) --> [loves].

intrans_verb(X,live(X)) --> [lives].

interpret(S, Logic) :- sentence(Logic, S,[]).

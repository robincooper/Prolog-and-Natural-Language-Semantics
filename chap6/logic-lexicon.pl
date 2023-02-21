% A sample lexicon.  Arity n represented by a list of length n.

predicate(run,[_]).
predicate(man,[_]).
predicate(woman,[_]).
predicate(program,[_]).
predicate(student,[_]).


predicate(want,[_,_]).
predicate(snore,[_]).
predicate(wake,[_]).
predicate(walk,[_]).
predicate(talk,[_]).
predicate(student,[_]).
predicate(clever,[_]).
predicate(fun,[_]).
predicate(swim,[_]).
predicate(halt,[_]).


predicate(love,[_,_]).
predicate(write,[_,_]).
predicate(say,[_,_]).

constant(a).
constant(b).
constant(c).

constant(j).
constant(john).
constant(mary).
constant(terry).
constant(shrdlu).


/* Quantifiers do not properly belong in the lexicon but we will want
to add more quantifiers as we extend predicate calculus */

quantifier(every).
quantifier(some).


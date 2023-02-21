/*  A simple implementation of discourse representation using simple drs's */

/*  You may need to define member and append, depending on the prolog
you are using */

s(DRSIn,DRSOut) --> np(DRSIn,DRS1,X),vp(DRS1,DRSOut,X^Cond).

np(DRSIn,DRSOut,X) -->
	det(DRSIn,DRS1), noun(DRS1,DRS2,X^Cond),optrel(DRS2,DRSOut,X^Cond1).
	
np(DRSIn,DRSOut,X) -->
	proper_noun(DRSIn,DRSOut,X).

np(DRSIn,DRSOut,X) -->
	pronoun(DRSIn,DRSOut,X).

optrel(DRS,DRS,X^_) --> [].
optrel(DRSIn,DRSOut,X^Cond) -->
	[that],vp(DRSIn,DRSOut,X^Cond).
	
vp(DRSIn,DRSOut,X^Cond) -->
	trans_verb(DRSIn,DRS1,Y^X^Cond),np(DRS1,DRSOut,Y).
vp(DRSIn,DRSOut,X^Cond) -->
	intrans_verb(DRSIn,DRSOut,X^Cond).
 	
det(DRS,DRS) -->
	[a].
	
noun([Dom,Constr],[[X|Dom],[man(X),male(X)|Constr]],X^man(X)) -->
	[man].
noun([Dom,Constr],[[X|Dom],[woman(X),female(X)|Constr]],X^woman(X)) -->
	[woman].
noun([Dom,Constr],[[X|Dom],[donkey(X),neuter(X)|Constr]],X^donkey(X)) -->
	[donkey].

proper_noun(DRSIn,DRSOut,X) -->
	[john],{check_add(X,[named(X,john),male(X)],DRSIn,DRSOut)}.
proper_noun(DRSIn,DRSOut,X) -->
	[mary],{check_add(X,[named(X,mary),female(X)],DRSIn,DRSOut)}.


pronoun(DRS,DRS,X) -->
	[he], {check(X,[male(X)],DRS)}.
pronoun(DRS,DRS,X) -->
	[she], {check(X,[female(X)],DRS)}.
pronoun(DRS,DRS,X) -->
	[it], {check(X,[neuter(X)],DRS)}.
	
trans_verb([Dom,Constr],[Dom,[love(X,Y)|Constr]],Y^X^love(X,Y)) -->
	[loves].
trans_verb([Dom,Constr],[Dom,[beat(X,Y)|Constr]],Y^X^beat(X,Y)) -->
	[beats].
trans_verb([Dom,Constr],[Dom,[own(X,Y)|Constr]],Y^X^own(X,Y)) -->
	[owns].
	
intrans_verb([Dom,Constr],[Dom,[live(X)|Constr]],X^live(X)) -->
	[lives].
	
	
drs_sent(Sent,DRSIn,DRSOut) :-
	s(DRSIn,DRSOut,Sent,[]).
	
drs([],DRS,DRS).
drs([Sent|Disc],DRSIn,DRSOut) :-
	drs_sent(Sent,DRSIn,DRS1),
	drs(Disc,DRS1,DRSOut).
	
drs(Disc,DRS) :-
	drs(Disc,[[],[]],DRS).
	
check(X,Cs,[Dom,Constr]) :-
	member(X,Dom),
	strict_members(Cs,Constr).

check_add(X,Cs,DRS,DRS) :-
	check(X,Cs,DRS),!.
check_add(X,Cs,[Dom,Constr],[[X|Dom],NewConstr]) :-
	append(Cs,Constr,NewConstr).
	
strict_member(X,[Y|L]) :-
	X == Y.
strict_member(X,[_|L]) :-
	strict_member(X,L).
	
strict_members([],_).
strict_members([X|L],L1) :-
	strict_member(X,L1),
	strict_members(L,L1).

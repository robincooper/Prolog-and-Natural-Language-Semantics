/*  A simple implementation of discourse representation using complex drs's 
for universal quantification*/

/*  You may need to define member and append, depending on the prolog
you are using */

:- op(150,xfy,=>).

s(Gl_DRSIn,DRSIn,DRSOut,Gl_DRSOut,LocList,LocList) --> 
	np(Gl_DRSIn,DRSIn,DRS1,X,Gl_DRS1,[],LocList1),
	vp(Gl_DRS1,DRS1,DRSOut,X^Cond,Gl_DRSOut,LocList1,_).
s(Gl_DRSIn,DRSIn,DRSOut,Gl_DRSOut,LocList,LocList) --> 
	qnp(Gl_DRSIn, DRSIn,DRSOut,X^P^(Q=>P),Gl_DRS1,[],LocList1),
	vp(Gl_DRS1,Q,P,X^Cond,Gl_DRSOut,LocList1,_).

np(Gl_DRSIn,DRSIn,DRSOut,X,Gl_DRSOut,LocListIn,LocListOut) -->
	det(Gl_DRSIn,DRSIn,DRS1,Gl_DRS1), 
	noun(Gl_DRS1,DRS1,DRS2,X^Cond,Gl_DRS2,[],LocList1),
	optrel(Gl_DRS2,DRS2,DRSOut,X^Cond1,Gl_DRSOut,LocList1,_),
	{append(LocListIn,LocList1,LocListOut)}.
	
qnp(Gl_DRSIn,DRSIn,DRSOut,X^P^DRS,Gl_DRSOut,LocListIn,LocListOut) -->
	det(Gl_DRSIn,DRSIn,DRSOut,Q^P^DRS,Gl_DRS1),
	noun(Gl_DRS1,DRSIn,DRS1,X^Cond,Gl_DRS2,[],LocList1),
	optrel(Gl_DRS2,DRS1,Q,X^Cond1,Gl_DRSOut,LocList1,_),
	{append(LocListIn,LocList1,LocListOut)}.
	
np(Gl_DRSIn,DRSIn,DRSOut,X,Gl_DRSOut,LocListIn,LocListOut) -->
	proper_noun(Gl_DRSIn,DRSIn,DRSOut,X,Gl_DRSOut,LocListIn,LocListOut).

np(Gl_DRSIn,DRSIn,DRSOut,X,Gl_DRSOut,LocListIn,LocListOut) -->
	pronoun(Gl_DRSIn,DRSIn,DRSOut,X,Gl_DRSOut,LocListIn,LocListOut).

optrel(Gl_DRS,DRS,DRS,X^_,Gl_DRS,LocList,LocList) --> [].
optrel(Gl_DRSIn,DRSIn,DRSOut,X^Cond,Gl_DRSOut,LocListIn,LocListIn) -->
	[that],vp(Gl_DRSIn,DRSIn,DRSOut,X^Cond,Gl_DRSOut,LocListIn,_).
	
vp(Gl_DRSIn,DRSIn,DRSOut,X^Cond,Gl_DRSOut,LocListIn,LocListOut) -->
	trans_verb(Gl_DRSIn,DRSIn,DRS1,Y^X^Cond,Gl_DRS1),
	np(Gl_DRS1,DRS1,DRSOut,Y,Gl_DRSOut,LocListIn,LocListOut).
vp(Gl_DRSIn,DRSIn,DRSOut,X^Cond,Gl_DRSOut,LocListIn,LocListOut) -->
	trans_verb(Gl_DRSIn,Q,P,Y^X^Cond,Gl_DRS1),
	qnp(Gl_DRS1,DRSIn,DRSOut,Y^P^(Q=>P),Gl_DRSOut,LocListIn,LocListOut).
vp(Gl_DRSIn,DRSIn,DRSOut,X^Cond,Gl_DRSOut,LocList,LocList) -->
	intrans_verb(Gl_DRSIn,DRSIn,DRSOut,X^Cond,Gl_DRSOut).
 	
det(Gl_DRS,DRS,DRS,Gl_DRS) -->
	[a].
	
det(Gl_DRS,[Dom,Constr],[Dom,[Q=>P|Constr]],Q^P^(Q=>P),Gl_DRS) --> 
	[every].
	
noun(Gl_DRS,[Dom,Constr],[[X|Dom],[man(X),male(X)|Constr]],X^man(X),Gl_DRS,
	LocListIn,[X|LocListIn]) -->
	[man].
noun(Gl_DRS,[Dom,Constr],[[X|Dom],[woman(X),female(X)|Constr]],X^woman(X),Gl_DRS,
	LocListIn,[X|LocListIn]) -->
	[woman].
noun(Gl_DRS,[Dom,Constr],[[X|Dom],[donkey(X),neuter(X)|Constr]],X^donkey(X),Gl_DRS,
	LocListIn,[X|LocListIn]) -->
	[donkey].

proper_noun(Gl_DRSIn,DRS,DRS,X,Gl_DRSOut,LocListIn,[X|LocListIn]) -->
	[john],{check_add(X,[named(X,john),male(X)],Gl_DRSIn,Gl_DRSOut),
	\+ strict_member(X,LocListIn)}.
proper_noun(Gl_DRSIn,DRS,DRS,X,Gl_DRSOut,LocListIn,[X|LocListIn]) -->
	[mary],{check_add(X,[named(X,mary),female(X)],Gl_DRSIn,Gl_DRSOut),
	\+ strict_member(X,LocListIn)}.


pronoun(Gl_DRS,DRS,DRS,X,Gl_DRS,LocListIn,[X|LocListIn]) -->
	[he], {(check(X,[male(X)],DRS);check(X,[male(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS,DRS,DRS,X,Gl_DRS,LocListIn,[X|LocListIn]) -->
	[him], {(check(X,[male(X)],DRS);check(X,[male(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS,DRS,DRS,X,Gl_DRS,LocListIn,[X|LocListIn]) -->
	[she], {(check(X,[female(X)],DRS);check(X,[female(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS,DRS,DRS,X,Gl_DRS,LocListIn,[X|LocListIn]) -->
	[her], {(check(X,[female(X)],DRS);check(X,[female(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS,DRS,DRS,X,Gl_DRS,LocListIn,[X|LocListIn]) -->
	[it], {(check(X,[neuter(X)],DRS);check(X,[neuter(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
	
trans_verb(Gl_DRS,[Dom,Constr],[Dom,[love(X,Y)|Constr]],Y^X^love(X,Y),Gl_DRS) -->
	[loves].
trans_verb(Gl_DRS,[Dom,Constr],[Dom,[beat(X,Y)|Constr]],Y^X^beat(X,Y),Gl_DRS) -->
	[beats].
trans_verb(Gl_DRS,[Dom,Constr],[Dom,[own(X,Y)|Constr]],Y^X^own(X,Y),Gl_DRS) -->
	[owns].
trans_verb(Gl_DRS,[Dom,Constr],[Dom,[know(X,Y)|Constr]],Y^X^knows(X,Y),Gl_DRS) -->
	[knows].
	
intrans_verb(Gl_DRS,[Dom,Constr],[Dom,[live(X)|Constr]],X^live(X),Gl_DRS) -->
	[lives].
	
	
drs_sent(Sent,Gl_DRSIn,DRSIn,DRSOut,Gl_DRSOut,LocListIn,LocListOut) :-
	s(Gl_DRSIn,DRSIn,DRSOut,Gl_DRSOut,LocListIn,LocListOut,Sent,[]).
	
drs([],Gl_DRS,DRS,DRS,Gl_DRS).
drs([Sent|Disc],Gl_DRSIn,DRSIn,DRSOut,Gl_DRSOut) :-
	drs_sent(Sent,Gl_DRSIn,DRSIn,DRS1,Gl_DRS1,[],_),
	drs(Disc,Gl_DRS1,DRS1,DRSOut,Gl_DRSOut).
	
drs(Disc,DRS) :-
	drs(Disc,[[],[]],[[],[]],DRSOut,Gl_DRSOut),
	merge_drs(Gl_DRSOut,DRSOut,DRS).
	
merge_drs([Dom1,Constr1],[Dom2,Constr2],[Dom,Constr]) :-
	append(Dom1,Dom2,Dom),
	append(Constr1,Constr2,Constr).
	
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
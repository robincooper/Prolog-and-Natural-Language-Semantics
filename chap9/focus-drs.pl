/*  A simple implementation of discourse representation using complex drs's 
for universal quantification*/

/*  You may need to define member and append, depending on the prolog
you are using */

:- op(150,xfy,=>).
:- op(100,xfy,:).

s(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,LocList-LocList,FocIn-FocOut) --> 
	np(Gl_DRSIn-Gl_DRS1,DRSIn-DRS1,X,[]-LocList1,FocIn-Foc1),
	vp(Gl_DRS1-Gl_DRSOut,DRS1-DRSOut,X^Cond,LocList1-_,Foc1-FocOut).
s(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,LocList-LocList,FocIn-FocOut) --> 
	qnp(Gl_DRSIn-Gl_DRS1, DRSIn-DRSOut,X^P^(Q=>P),[]-LocList1,FocIn-Foc1),
	vp(Gl_DRS1-Gl_DRSOut,Q-P,X^Cond,LocList1-_,Foc1-FocOut).

np(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,_:X,LocListIn-LocListOut,FocIn-FocOut) -->
	det(Gl_DRSIn-Gl_DRS1,DRSIn-DRS1,FocIn-Foc1), 
	noun(Gl_DRS1-Gl_DRS2,DRS1-DRS2,X^Cond,[]-LocList1,Foc1-Foc2),
	optrel(Gl_DRS2-Gl_DRSOut,DRS2-DRSOut,X^Cond1,LocList1-_,Foc2-FocOut),
	{append(LocListIn,LocList1,LocListOut)}.
	
qnp(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,_:X^P^DRS,LocListIn-LocListOut,FocIn-FocOut) -->
	det(Gl_DRSIn-Gl_DRS1,DRSIn-DRSOut,Q^P^DRS,FocIn-Foc1),
	noun(Gl_DRS1-Gl_DRS2,DRSIn-DRS1,X^Cond,[]-LocList1,Foc1-Foc2),
	optrel(Gl_DRS2-Gl_DRSOut,DRS1-Q,X^Cond1,LocList1-_,Foc2-FocOut),
	{append(LocListIn,LocList1,LocListOut)}.
	
np(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X,LocListIn-LocListOut,FocIn-FocOut) -->
	proper_noun(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X,LocListIn-LocListOut,FocIn-FocOut).

np(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X,LocListIn-LocListOut,FocIn-FocOut) -->
	pronoun(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X,LocListIn-LocListOut,FocIn-FocOut).

optrel(Gl_DRS-Gl_DRS,DRS-DRS,X^_,LocList-LocList,Foc-Foc) --> [].
optrel(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X^Cond,LocListIn-LocListIn,FocIn-FocOut) -->
	[that],vp(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X^Cond,LocListIn-_,FocIn-FocOut).
	
vp(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X^Cond,LocListIn-LocListOut,FocIn-FocOut) -->
	trans_verb(Gl_DRSIn-Gl_DRS1,DRSIn-DRS1,Y^X^Cond,FocIn-Foc1),
	np(Gl_DRS1-Gl_DRSOut,DRS1-DRSOut,Y,LocListIn-LocListOut,Foc1-FocOut).
vp(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X^Cond,LocListIn-LocListOut,FocIn-FocOut) -->
	trans_verb(Gl_DRSIn-Gl_DRS1,Q-P,Y^X^Cond,FocIn-Foc1),
	qnp(Gl_DRS1-Gl_DRSOut,DRSIn-DRSOut,Y^P^(Q=>P),LocListIn-LocListOut,Foc1-FocOut).
vp(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X^Cond,LocList-LocList,FocIn-FocOut) -->
	intrans_verb(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,X^Cond,FocIn-FocOut).
 	
det(Gl_DRS-Gl_DRS,DRS-DRS,Foc-Foc) -->
	[a].
	
det(Gl_DRS-Gl_DRS,[Dom,Constr]-[Dom,[Q=>P|Constr]],Q^P^(Q=>P),Foc-Foc) --> 
	[every].
	
noun(Gl_DRS-Gl_DRS,[Dom,Constr]-[[X|Dom],[man(X),male(X)|Constr]],X^man(X),
	LocListIn-[X|LocListIn],Foc-Foc) -->
	[man].
noun(Gl_DRS-Gl_DRS,[Dom,Constr]-[[X|Dom],[woman(X),female(X)|Constr]],X^woman(X),
	LocListIn-[X|LocListIn],Foc-Foc) -->
	[woman].
noun(Gl_DRS-Gl_DRS,[Dom,Constr]-[[X|Dom],[donkey(X),neuter(X)|Constr]],X^donkey(X),
	LocListIn-[X|LocListIn],Foc-Foc) -->
	[donkey].

proper_noun(Gl_DRSIn-Gl_DRSOut,DRS-DRS,_:X,LocListIn-[X|LocListIn],Foc-Foc) -->
	[john],{check_add(X,[named(X,john),male(X)],Gl_DRSIn,Gl_DRSOut),
	\+ strict_member(X,LocListIn)}.
proper_noun(Gl_DRSIn-Gl_DRSOut,DRS-DRS,_:X,LocListIn-[X|LocListIn],Foc-Foc) -->
	[mary],{check_add(X,[named(X,mary),female(X)],Gl_DRSIn,Gl_DRSOut),
	\+ strict_member(X,LocListIn)}.


pronoun(Gl_DRS-Gl_DRS,DRS-DRS,R:X,LocListIn-[X|LocListIn],
	(Foci,Anaphors,NewInfo)-(Foci,[X|Anaphors],NewInfo)) -->
	[he], 
	{r1(Foci,R:X),
	(strict_check(X,[male(X)],DRS);strict_check(X,[male(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS-Gl_DRS,DRS-DRS,R:X,LocListIn-[X|LocListIn],
	(Foci,Anaphors,NewInfo)-(Foci,[X|Anaphors],NewInfo)) -->
	[him], 
	{r1(Foci,R:X),
	(strict_check(X,[male(X)],DRS);strict_check(X,[male(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS-Gl_DRS,DRS-DRS,R:X,LocListIn-[X|LocListIn],
	(Foci,Anaphors,NewInfo)-(Foci,[X|Anaphors],NewInfo)) -->
	[she], 
	{r1(Foci,R:X),
	(strict_check(X,[female(X)],DRS);strict_check(X,[female(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS-Gl_DRS,DRS-DRS,R:X,LocListIn-[X|LocListIn],
	(Foci,Anaphors,NewInfo)-(Foci,[X|Anaphors],NewInfo)) -->
	[her], 
	{r1(Foci,R:X),
	(strict_check(X,[female(X)],DRS);strict_check(X,[female(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
pronoun(Gl_DRS-Gl_DRS,DRS-DRS,R:X,LocListIn-[X|LocListIn],
	(Foci,Anaphors,NewInfo)-(Foci,[X|Anaphors],NewInfo)) -->
	[it], 
	{r1(Foci,R:X),
	(strict_check(X,[neuter(X)],DRS);strict_check(X,[neuter(X)],Gl_DRS)),
	\+ strict_member(X,LocListIn)}.
	
trans_verb(Gl_DRS-Gl_DRS,[Dom,Constr]-[Dom,[love(exp:X,th:Y)|Constr]],
	th:Y^exp:X^love(exp:X,th:Y),
	(Foci,Anaphors,NewInfo)-(Foci,Anaphors,[love(exp:X,th:Y)|NewInfo])) -->
	[loves].
trans_verb(Gl_DRS-Gl_DRS,[Dom,Constr]-[Dom,[beat(ag:X,th:Y)|Constr]],
	th:Y^ag:X^beat(ag:X,th:Y),
	(Foci,Anaphors,NewInfo)-(Foci,Anaphors,[beat(ag:X,th:Y)|NewInfo])) -->
	[beats].
trans_verb(Gl_DRS-Gl_DRS,[Dom,Constr]-[Dom,[own(ag:X,th:Y)|Constr]],
	th:Y^ag:X^own(ag:X,th:Y),
	(Foci,Anaphors,NewInfo)-(Foci,Anaphors,[own(ag:X,th:Y)|NewInfo])) -->
	[owns].
trans_verb(Gl_DRS-Gl_DRS,[Dom,Constr]-[Dom,[know(exp:X,th:Y)|Constr]],
	th:Y^exp:X^knows(exp:X,th:Y),
	(Foci,Anaphors,NewInfo)-(Foci,Anaphors,[know(exp:X,th:Y)|NewInfo])) -->
	[knows].
	
intrans_verb(Gl_DRS-Gl_DRS,[Dom,Constr]-[Dom,[live(th:X)|Constr]],
	th:X^live(th:X),
	(Foci,Anaphors,NewInfo)-(Foci,Anaphors,[live(th:X)|NewInfo])) -->
	[lives].
	
	
drs_sent(Sent,Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,LocListIn-LocListOut,FocIn-FocOut) :-
	s(Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,LocListIn-LocListOut,FocIn-Foc1,Sent,[]),
	update_foc(Foc1,FocOut).
	
drs([],Gl_DRS-Gl_DRS,DRS-DRS,Foc-Foc).
drs([Sent|Disc],Gl_DRSIn-Gl_DRSOut,DRSIn-DRSOut,FocIn-FocOut) :-
	drs_sent(Sent,Gl_DRSIn-Gl_DRS1,DRSIn-DRS1,[]-_,FocIn-Foc1),
	drs(Disc,Gl_DRS1-Gl_DRSOut,DRS1-DRSOut,Foc1-FocOut).
	
drs(Disc,DRS) :-
	drs(Disc,[[],[]]-Gl_DRSOut,[[],[]]-DRSOut,((nil,nil,[],[]),[],[])-FocOut),
	merge_drs(Gl_DRSOut,DRSOut,DRS).
	
drs(Disc,DRS,FocOut) :-
	drs(Disc,[[],[]]-Gl_DRSOut,[[],[]]-DRSOut,((nil,nil,[],[]),[],[])-FocOut),
	merge_drs(Gl_DRSOut,DRSOut,DRS).
	
merge_drs([Dom1,Constr1],[Dom2,Constr2],[Dom,Constr]) :-
	append(Dom1,Dom2,Dom),
	append(Constr1,Constr2,Constr).
	
r1((_,ActorFocus,_,_),ag:ActorFocus).
r1((DiscourseFocus,_,_,_),th:DiscourseFocus).
r1((DiscourseFocus,_,_,_),exp:DiscourseFocus).
r1((_,_,AltFocusList,_), _:X) :-
	member(X,AltFocusList).
r1((_,_,_,[X|FocusStack]), _:X).

update_foc((FociIn,Anaphors,NewInfo),(FociOut,[],[]) ) :-
	expected_focus(FociIn,FociOut,Anaphors,NewInfo),!.
update_foc((FociIn,Anaphors,NewInfo),(FociOut,[],[]) ) :-
	focus(FociIn,FociOut,Anaphors,NewInfo).
	
expected_focus((X,Y,[],[]),(DiscourseFocus,ActorFocus,AltFocusList,[]),
	_,[Cond|NewInfo]) :-
	X==nil,Y==nil,
	role(th,Cond,DiscourseFocus),
	role(ag,Cond,ActorFocus),
	other_roles(Cond,AltFocusList1),
	(\+ ActorFocus == nil -> append(AltFocusList1,[ActorFocus],AltFocusList);
	AltFocusList1=AltFocusList).
	
role(R,Cond,X) :-
	Cond =.. [Pred|Args],
	member(R:X,Args),!.
role(_,_,nil).

other_roles(Cond,L) :-
	Cond =.. [Pred|Args],
	bagof(X,R^(member(R:X,Args),\+(R=ag),\+(R=th)),L),!.
other_roles(_,[]).

focus((DFocusIn,AFocusIn,AltFLIn,FStackIn),(DFocusOut,AFocusIn,AltFLOut,FStackIn),
	Anaphors,[Cond|NewInfo]) :-
	strict_member(DFocusIn,Anaphors),
	strict_shared_member(Anaphors,AltFLIn,X),
	role(ag,Cond,A),
	(\+A==DFocusIn -> DFocusIn = DFocusOut; X = DFocusOut),
	potential_focus(DFocusIn,Cond,AltFLOut).
focus((DFocusIn,AFocusIn,AltFLIn,FStackIn),(DFocusIn,AFocusIn,AltFLOut,FStackIn),
	Anaphors,[Cond|NewInfo]) :- 
	strict_member(DFocusIn,Anaphors),!,
	potential_focus(DFocusIn,Cond,AltFLOut). 
focus((DFocusIn,AFocusIn,AltFLIn,FStackIn),(X,AFocusIn,AltFLOut,[DFocusIn|FStackIn]),
	[X],[Cond|NewInfo]) :-
	strict_member(X,AltFLIn),!,
	potential_focus(DFocusIn,Cond,AltFLOut).
focus((DFocusIn,AFocusIn,AltFLIn,[Y|FStackIn]),(X,AFocusIn,AltFLOut,FStackIn),
	[X],[Cond|NewInfo]) :-
	X==Y,!,
	potential_focus(DFocusIn,Cond,AltFLOut).
focus((DFocusIn,AFocusIn,AltFLIn,FStackIn),(DFocusIn,AFocusIn,AltFLOut,FStackIn),
	_,[Cond|NewInfo]) :-
	potential_focus(DFocusIn,Cond,AltFLOut).
	
potential_focus(DFocusIn,Cond,AltFLOut) :-
	Cond =..[_|Args],
	bagof(X,R^(member(R:X,Args),\+R=ag,\+X==DFocusIn),AltFLOut),!.
potential_focus(_,_,[]).

check(X,Cs,[Dom,Constr]) :-
	member(X,Dom),
	strict_members(Cs,Constr).
	
strict_check(X,Cs,[Dom,Constr]) :-
	strict_member(X,Dom),
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
	
strict_shared_member([X|L1],L2,X) :-
	strict_member(X,L2).
strict_shared_member([_|L1],L2,X) :-
	strict_shared_member(L1,L2,X).

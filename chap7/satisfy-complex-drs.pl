:- op(150,xfy,=>).

satisfy(Sit,[Dom,Constr]) :-
	support_all(Sit,Constr).
	
support_all(_,[]).
support_all(Sit,[X|L]) :-
	support(Sit,X),
	support_all(Sit,L).
	
support(Sit, [Dom,Constr]=>DRS) :-
	!,bagof(Dom,support_all(Sit,Constr),Bindings),
	satisfy_all(Sit,Bindings,Dom,DRS).
support(Sit,Fact) :-
	Clause =.. [Sit,Fact],
	Clause.

satisfy_all(_,[],_,_).
satisfy_all(Sit,[B|Bindings],Dom,DRS) :-
	\+ \+ (B=Dom,satisfy(Sit,DRS)),
	satisfy_all(Sit,Bindings,Dom,DRS).

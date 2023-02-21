satisfy(Sit,[Dom,Constr]) :-
	support_all(Sit,Constr).
	
support_all(_,[]).
support_all(Sit,[X|L]) :-
	support(Sit,X),
	support_all(Sit,L).
	
support(Sit,Fact) :-
	Clause =.. [Sit,Fact],
	Clause.

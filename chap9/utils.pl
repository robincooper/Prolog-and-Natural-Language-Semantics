
%
%
%

member(X,[X | _]).
member(X,[_ | Y]):-
	member(X,Y).

append([],X,X).
append([X | Y],Z,[X | R]) :-
    append(Y,Z,R).

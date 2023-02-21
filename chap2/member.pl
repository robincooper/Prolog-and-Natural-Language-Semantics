
%
%  satify-pc.pl requires member to be defined if you do not
%  already have it defined from a library you can use this
%  one
%

member(X,[X | _]).
member(X,[_ | Y]):-
	member(X,Y).


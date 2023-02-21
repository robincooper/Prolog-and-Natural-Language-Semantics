% Title: reduce.pl
% Authors:  Guy Barry and David Beaver

reduce(X^P*Y,Q) :-
	sub(Y,X,P,Q).

% sub(Val,Var,InTerm,OutTerm) :-
%	OutTerm is a copy of InTerm where the variable Var has been
%	replaced by the term or variable Val and all other variables
%	remain unchanged (given that Var does not occur in Val).  
sub(Val,Var,InTerm,OutTerm) :-
	var(Var),
	bagof(  Term,
		Var^(Var=Val,Term=InTerm),
		[OutTerm] ).





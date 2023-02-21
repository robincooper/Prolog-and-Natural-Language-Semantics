/*******************************************************************************

   		Simplified Version of Scope Generation Algorithm (no.1)

		From:	Hobbs & Shieber, (1987) An Algorithm for Generating
			Quantifier Scopings. Computational Linguistics, 13,
			 No 1-2, 47-63

                This program contains the heart of H&S's recursive technique
                for dealing with multiply quantified sentences.

		It might be of some help to look at the predicates test/1,
	test1/0, test2/0, test3/0, test4/0 and test5/0 near the end of the file.

*******************************************************************************/

/*------------------------------------------------------------------------------
			Representation of wffs:

A wff of the form 'p(arg1,...,argn)' is represented as the Prolog term
wff(p,[arg1',...argn']) where argi' is the encoding of the
subexpression argi.

A constant term is represented by the homonymous Prolog constant.

A complex term is represented by the Prolog term
term(quant,var,restrict') where restrict' is the encoding of the wff
that forms the restriction of the quantifier. Please note that the variable
"var" is *not* represented by a Prolog variable: use x not X.

Thus we have:

  Every man loves a woman.

 	loves(<every r man(r)>,<a s woman(s)>)
  
  This wff is representated in Prolog as: 
 		wff(loves, [arg1', arg2']) 
 	where	arg1' is: term(every, r, wff(man, [r]))
 		arg2' is: term(a, s, wff(woman, [s])) 
------------------------------------------------------------------------------*/

% gen(Form, ScopedForm) 
% ===================== 
% 
%	Form		==> a wff with in-place complex terms 
%	ScopedForm	<== a full scoping of Form 

gen(Form, ScopedForm) :-
	apply_terms(Form, ScopedForm).

% apply_terms(Form, ScopedForm) 
% ======================================= 
% 
%	Form		==> a wff with in-place complex terms 
%	ScopedForm	<== a full scoping of Form 
% 
%	Applies one or more terms to the Form alone 

apply_terms(Form, Form) :-
	\+ term(Form, Term), !.

apply_terms(Form, ScopedForm) :-
	applicable_term(Form, Term),
	apply(Term, Form, AppliedForm),
	apply_terms(AppliedForm, ScopedForm).

% apply(Term, Form, NewForm) 
% ========================== 
% 
%	Term		==> a complex term 
%	Form		==> the wff to apply Term to 
%	NewForm		<== Form with the quantifier wrapped around it 

apply(term(Quant, Var, Restrict),
		Body,
		wff(Quant, [Var, Restrict, OutBody])) :-
	subst(Var, term(Quant, Var, Restrict), Body, OutBody).

% applicable_term(Form, Term) 
% =========================== 
% 
%	Form		==> an expression in the logical form language 
%	Term		<== any term in Form
%
% In this version of the program, we simply allow any Term contained
% within Form to be applicable - so we can just define it by means
% of the term/2 already used in apply_terms/2

applicable_term(Form, Term) :-
	term(Form,Term).

/*******************************************************************************

My implementation of the low-level predicates not included in the appendix of
the paper. (all 3 programs identical below this line)

*******************************************************************************/

% quantifer(Quant) 
% ================ 
% 
%	Quant		==> a valid quantifier 

quantifier(every).
quantifier(most).
quantifier(some).
quantifier(each).
quantifier(a_few).

% subst(New, Old, OldForm, NewForm) 
% ================================= 
% 
%	New		==> A pattern to substitute for Old 
%	Old		==> A pattern to be replaced by New 
%	OldForm		==> a wff with in-place complex terms 
%	NewForm		<== OldForm with each occurrence of Old replaced by New 

subst(New, Old, Old, New) :- !.
subst(New, Old, wff(Quant, ArgList), wff(Quant, NewArgList)) :- !,
	subst(New, Old, ArgList, NewArgList).
subst(New, Old, term(Quant, Var, Restrict), term(Quant, Var, NewRestrict)) :- !,
	subst(New, Old, Restrict, NewRestrict).

subst(B, A, [A|T], [B|NT]) :- !,
	subst(B, A, T, NT).
subst(B, A, [H|T], [NH|NT]) :- !,
	subst(B, A, H, NH),
	subst(B, A, T, NT).
subst(B, A, Form, Form).

% term(Form, Term) 
% ================ 
% 
%	Form		==> a wff or complex term 
%	Term		<== a complex term contained in Form 
% 
%	Extracts a term from Form. 

% If Form is a wff, a term of form is a term of its argument list. 
term(wff(Pred, ArgList), Term) :-
	term(ArgList, Term).

% If Form is a term, then it is a term. 
term(term(Quant, Var, Restrict), term(Quant, Var, Restrict)).

% If Form is a term, then a term is a term of its restriction. 
term(term(Quant, Var, Restrict), Term) :-
	term(Restrict, Term).

% If Form is an argument list, a term is a term of its head or of its tail. 
term([H|T], Term) :-
	term(H, Term);
	term(T, Term).

% free_in(VarList, Restriction) 
% ============================= 
% 
%	VarList		==> a list of variables which should be free in Restriction 
%	Restriction	==> a wff or term 
% 
%	Succeeds if each variable in VarList is free in Restriction 

free_in([], R) :- !,
	fail.
free_in([H], R) :- !,
	free_in(H, R).
free_in([H|T], R) :- !,
	free_in(H, R),
	free_in(T, R).

% Var is a single variable. If Restriction is a wff, Var is free in it if it  
% is free in its argument list. 
free_in(Var, wff(Pred, ArgList)) :-
	free_in(Var, ArgList).

% Var is a single variable. If Restriction is an argument list, Var is free 
% in it if it is free in the head or the tail of that argument list. 
free_in(Var, [H|T]) :-
	free_in(Var, H);
	free_in(Var, T).

% Var is a single variable. If Restriction is a complex term, Var is free in 
% it if Var is not the variable quantified over and if Var is free in the 
% restriction of the complex term. 
free_in(Var, term(Quant, Var1, Restriction)) :-
	\+ Var = Var1,
	free_in(Var, Restriction).

% Var is a single variable. If Restriction is just this variable, then Var 
% is free in Restriction 
free_in(Var, Var).

% Undefined predicate calls: 

% opaque(Pred, ArgIndex). 

opaque(not, 1).
opaque(seek, 2).

% convert(IOLang, PrologLang) 
% =========================== 
%
%	IOLang		<=> A wff of the original language with complex
%			    terms of the form <Q V R> represented as
%			    Q-V-R.
%	PrologLang	<=> A wff of the language used as input to the
%			    gen/2 predicate.

% This predicate was just written to help me convert from the language
% defined in the paper and the language used by the Prolog program.

convert(Quant-Var-IORestrict, term(Quant, Var, PrologRestrict)) :- !,
	convert(IORestrict, PrologRestrict).
convert([IOH|IOArgList], [H|ArgList]) :- !,
	convert(IOH, H),
	convert(IOArgList, ArgList).
convert([], [])  :- !.
convert(Atom, Atom) :-
	atom(Atom), !.
convert(IOLang, wff(Pred, ArgList)) :-
	var(IOLang), !,
	convert(IOArgList, ArgList),
	IOLang =.. [Pred|IOArgList].
convert(IOLang, wff(Pred, ArgList)) :-
	nonvar(IOLang), !,
	IOLang =.. [Pred|IOArgList],
	convert(IOArgList, ArgList).

% test(InputForm)
% ===============
%
%	InputForm	==> An input form to have its quantifiers/opaque
%			    predicates scoped.

% This is just for demonstrating that the program works. It asserts all 
% generated forms so that they may be examined (or counted) after execution.
% See test1, test2, test3, test4 and test5 for examples of the use of test/1.

test(IO) :-
	abolish(wff, 2),
	write('Input structure: '), nl,
	pretty_print(IO), nl,
	convert(IO, Form),
	gen(Form, ScopedForm),
	assert(ScopedForm),
	convert(ScopedIO, ScopedForm),
	write('A possible scoping: '), nl,
	pretty_print(ScopedIO), nl,
	fail.

% pretty_print(Tree)
% ==================
%
% 	Tree		==> An i/o structure to be written nicely.

% Someone might like to write a decent pretty printing predicate.  The
% main thing appears to me to be getting a whole wff on a single screen. 

pretty_print(Tree) :-
	write(Tree), nl.

% test1
% =====
%
% Generates scopings for "Every representative of some company saw most samples".

test1 :-
	test(	see(	every-r-and(	representative(r),
					of(r, (some-c-company(c)))),
			most-s-sample(s))	).
test0 :-
	test(arrived(every-r-and(representative(r),
				of(r, (some-c-company(c)))))).

% test2
% =====
%
% Generates scopings for "Every man saw most samples".

test2 :-
	test(	see(	every-r-man(r),
			most-s-sample(s))	).

% test3
% =====
%
% Generates scopings for "Some representative of every department in most
% companies saw a-few samples of each product".

test3 :-
	test(	see(	some-r-and(	rep(r),
					of(r, every-d-and(	dept(d),
								in(d, most-c-co(c))))),
			a_few-s-and(	samp(s),
					of(s, each-p-prod(p))))	).

% test4
% =====
%
% Generates scopings for "Every man is not here".

test4 :-
	test(	not(	is(	every-m-man(m),
				here(m)))	).

% test5
% =====
%
% Generates scopings for "John seeks a unicorn".

test5 :-
	test(	seek(	john,
			some-u-unicorn(u))	).




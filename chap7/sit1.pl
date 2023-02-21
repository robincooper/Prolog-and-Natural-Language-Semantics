sit1(run(a)).
sit1(man(a)).
sit1(run(b)).
sit1(man(b)).
sit1(run(c)).
sit1(woman(c)).
sit1(love(a,c)).

sit1(donkey(d)).
sit1(donkey(e)).
sit1(donkey(f)).

sit1(own(a,d)).
sit1(own(a,e)).
sit1(own(a,f)).
sit1(beat(a,d)).
sit1(beat(a,e)).

sit1(male(X)) :-
	sit1(man(X)).

sit1(neuter(X)) :-
	sit1(donkey(X)).

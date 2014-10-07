friend(X,Y) :- knows(X,Y); knows(Y,X).
knows_list(X,List) :- friend(X,Y), member(Y,List), !.
friendlist(X,List) :- bagof(Y,friend(X,Y),List).

c([P|People], Conspirators) :- 
	friendlist(P,Friends),
	subtract(People,Friends,Possible),
	c(Possible, C1),
	Conspirators = [P|C1].
c([_|People],Conspirators) :- c(People,Conspirators).
c([],[]).
 
check_list(Conspirators) :- 
	forall(
		person(Y), 
		(member(Y,Conspirators);knows_list(Y,Conspirators))
	).

testList(Possible) :- 
			c(Possible,Conspirators),
			check_list(Conspirators), !.

spider(X) :- person(X), friendlist(X,Possible), testList(Possible).
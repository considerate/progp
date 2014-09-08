fib(0,X,_,X).
fib(N,X,Y,F) :- N > 0,
				Prev is N-1,
				Z is X+Y,
				fib(Prev,Y,Z,F).

fib(N, F) :- fib(N, 0, 1, F).			


consonant(X):- member(X,"pqwrtsdfghjklzxcvbnm").
vowel(X):- member(X,"aoueiy").

suffix(S,L):-  append(_,S,L).

pirate(X,L) :- vowel(X), L = [X], !.
pirate(X,L) :- consonant(X), L = [X,111,X], !.
pirate(X,L) :- L = [X], !.

piratelist([], []).
piratelist([X|XS], [X|Out]) :- vowel(X), piratelist(XS, Out), !.
piratelist([X|XS], [X|Out]) :- consonant(X), Out = [111,X|Out1], piratelist(XS, Out1), !.
piratelist([X|XS], [X|Out]) :- not(consonant(X)), piratelist(XS, Out), !.

rovarsprak(Text,RovarText) :- nonvar(Text), maplist(pirate, Text, Lists), append(Lists,RovarText), !.
rovarsprak(Text,RovarText) :- nonvar(RovarText),piratelist(Text, RovarText), !.

numwords([],I) :- I is 0.
numwords([X],I) :- code_type(X,alpha), I is 1, !.
numwords([_],I) :- I is 0.
numwords([X,Y|XS],I) :- code_type(X,alpha),
						not(code_type(Y,alpha)),
						numwords(XS, I1),
						I is I1+1,
						!.
numwords([_,Y|XS],I) :- numwords([Y|XS],I).

alphastr([],[]).
alphastr([X], Out) :- code_type(X,alpha), Out = [X].
alphastr([X], Out) :- not(code_type(X,alpha)), Out = [].
alphastr([X|XS], Out) :- code_type(X,alpha), Out=[X|Rest], alphastr(XS, Rest), !.
alphastr([X|XS], Out) :- not(code_type(X,alpha)), alphastr(XS, Out), !.

medellangd(Text, AvgLen) :- numwords(Text,Words), Words > 0, alphastr(Text, Alpha), length(Alpha,AlphaLength), AvgLen is AlphaLength / Words, !.
medellangd(_, AvgLen) :- AvgLen = 0.0.

odds([], []).
odds([X],Odd) :- Odd = [X].
odds([X,_], Odd) :- Odd = [X].
odds([X,_|XS],Odd) :- Odd = [X|Odd1], odds(XS,Odd1). 

evens([],[]).
evens([_],[]).
evens([_|XS], Even) :- odds(XS, Even), !.


skyffla([],[]) :- !.
skyffla([X], Skyfflad) :- Skyfflad = [X], !.
skyffla(Lista, Skyfflad) :- odds(Lista, First), evens(Lista,Next), skyffla(Next, NextOrdered), append(First, NextOrdered, Skyfflad), !.

%% set_num(X,O) :- O = X+1.
%% set_char(X,L) :- L = 112.
%% set_chars(X,L) :- L = [112,111,112].
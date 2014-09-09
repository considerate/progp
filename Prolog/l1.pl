fib(0,X,_,X).
fib(N,X,Y,F) :- N > 0,
				Prev is N-1,
				Z is X+Y,
				fib(Prev,Y,Z,F).

fib(N, F) :- fib(N, 0, 1, F).			


vowel(97). %% 'a'
vowel(101). %% 'e'
vowel(105). %% 'i'
vowel(111). %% 'o'
vowel(117). %% 'u'
vowel(121). %% 'y'

rovarsprak([], []).
rovarsprak([X|XS], [X|Out]) :- not(vowel(X)), Out = [111,X|Out1], rovarsprak(XS, Out1), !.
rovarsprak([X|XS], [X|Out]) :- rovarsprak(XS, Out), !.


wordcounts([],L,W) :- L is 0, W is 0, !.
wordcounts([X],L,W) :- code_type(X,alpha), L is 1, W is 1, !.
wordcounts([_],L,W) :- L is 0, W is 0, !.
wordcounts([X,Y|XS],L,W) :-	not(code_type(Y,alpha)),
							code_type(X,alpha),
							wordcounts(XS,L0,W0),
							L is L0 + 1, 
							W is W0 + 1,
							!.
wordcounts([X|XS],L,W) :- 	code_type(X,alpha),
							wordcounts(XS,L0,W),
							L is L0 + 1,
							!.
wordcounts([_|XS],L,W) :- 	wordcounts(XS,L,W), !.

medellangd([], AvgLen) :- AvgLen = 0.0.
medellangd(Text, AvgLen) :- wordcounts(Text,L,W), AvgLen is L/W.

odds([], []).
odds([X],Odd) :- Odd = [X].
odds([X,_], Odd) :- Odd = [X].
odds([X,_|XS],[X|Odd]) :- odds(XS,Odd), !. 

evens([],[]).
evens([_],[]).
evens([_|XS], Even) :- odds(XS, Even), !.


skyffla([],[]) :- !.
skyffla([X], Skyfflad) :- Skyfflad = [X], !.
skyffla([X,Y], Skyfflad) :- Skyfflad = [X,Y], !.
skyffla(Lista, Skyfflad) :- odds(Lista, First), evens(Lista,Next), skyffla(Next, NextOrdered), append(First, NextOrdered, Skyfflad), !.

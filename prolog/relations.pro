:- initialization(consult('kinship.pro')).

%  #1

pred(X, Y) :- parent(X, Y).
pred(X, Y) :- parent(X, Z), pred(Z, Y).

%  #2

father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).

brotherOrSister(X, Y)  :- 
                   father(Z, X),  father(Z, Y),
                   mother(W, X),  mother(W, Y), 
                   X \= Y.

brother(X, Y)   :-  male(X), 
                    brotherOrSister(X, Y).


% #3
hasKids(X, Y) :- parent(X, A), parent(Y, A), 
                 X \= Y.

married(X, Y) :-
    setof(_, hasKids(X, Y), _).


%  #4

husband(X, Y) :- married(X, Y), male(X).


%  #5
cousin(X, Y) :-
    parent(P1, X), parent(P2, Y),
    brotherOrSister(P1, P2).



%  #6
single(X):- male(X),   \+parent(X,_).
single(X):- female(X), \+parent(X,_).

numOfChildren(X, 0) :- setof(N, single(X), N).
numOfChildren(X, N) :- setof(Y, parent(X,Y), Y), length(Y,N).

%  #7

nephewOrNiece(X, Y) :- parent(Z, X), brotherOrSister(Z, Y).

nephews(L, Y) :- bagof(Lst, nephewOrNiece(Lst, Y), Lst), permutation(Lst, L), !.



%  #8

allKids(F, M, Lst) :- bagof(Lst, (father(F, Lst), mother(M, Lst)), Lst).

family(Lst) :- nonvar(Lst), Lst = [F, M | T], allKids(F, M, Kids), permutation(Kids, T), !. 
family(Lst) :- var(Lst), Lst = [F, M | T], allKids(F, M, T).



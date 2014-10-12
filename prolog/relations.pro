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

% Tests
% | ?- brother(eugene, eugene).
%
% no
% | ?- brother(X, X).          
%
% no
% | ?- brother(X, max).
%
% X = robert ? ;
%
% X = eugene ? ;
%
% no
% | ?- brother(vivian, X).
%
% no


% #3
hasKids(X, Y) :- parent(X, A), parent(Y, A), 
                 X \= Y.

married(X, Y) :-
    setof(_, hasKids(X, Y), _).

%| ?- married(X, Y).
%
%X = albert
%Y = sarah ? ;
%
%X = alex
%Y = rosa ? ;
%
%X = amelia
%Y = peter ? ;
%
%X = ann
%Y = william ? ;
%
%X = donald
%Y = sarah ? ;
%
%X = eugene
%Y = rachel ? ;
%
%X = harry
%Y = laura ? ;
%
%X = henry
%Y = laura ? ;
%
%X = jack
%Y = mary ? ;
%
%и т.д.


%  #4

husband(X, Y) :- married(X, Y), male(X).

%| ?- husband(william, R).
%
%R = ann
%
%yes
%| ?- husband(thomas, R). 
%
%R = julie ? ;
%
%R = susanne
%
%yes

%  #5
cousin(X, Y) :-
    parent(P1, X), parent(P2, Y),
    brotherOrSister(P1, P2).

%| ?- cousin(natalie, Y).
%
%Y = helen ? ;
%
%Y = andrew ? ;
%
%Y = john ? ;
%
%no

%  #6
single(X):- male(X),   \+parent(X,_).
single(X):- female(X), \+parent(X,_).

numOfChildren(X, 0) :- setof(N, single(X), N).
numOfChildren(X, N) :- setof(Y, parent(X,Y), Y), length(Y,N).



% Tests
%numOfChildren(X, 0).
%
%X = andrew ? ;
%
%X = george ? ;
%
%X = helen ? ;
%
%X = jane ? ;
%
%X = john ? ;
%
%X = lily ? ;
%
%X = lucy ? ;
%
%X = monica ? ;
%
%X = natalie ? ;
%
%X = nina ? ;
%
%X = rebecca ? ;
%
%X = robert ? ;
%
%X = sonia ? ;
%
%X = steven ? ;
%
%X = victor ? ;
%
%X = vivian ? ;
%
%no


%| ?- numOfChildren(X, 2).
%
%X = albert ? ;
%
%X = eugene ? ;
%
%X = henry ? ;
%
%X = max ? ;
%
%X = rachel ? ;
%
%no



%  #7

nephewOrNiece(X, Y) :- parent(Z, X), brotherOrSister(Z, Y).

nephews(L, Y) :- bagof(Lst, nephewOrNiece(Lst, Y), Lst), permutation(Lst, L), !.
% Важно: permutation поставил для того, чтобы при работе предиката в режиме сопоставления
% племяшей можно было писать в любом порядке.
% Восклицательный знак в конце nephews оказался существенным, как ни странно. 
% Без него предикат вроде как завершает работу, но предлагает еще найти варианты (хотя ";" и "a" уже недоступны).

%| ?- nephews(X,Y).      
%
%X = [patrick]
%Y = alex ? ;
%
%X = [nina,george]
%Y = eugene ? ;
%
%X = [victor,rebecca]
%Y = max ? ;
%
%X = [steven]
%Y = michael ? ;
%
%X = [nina,george,victor,rebecca]
%Y = robert ? ;
%
%X = [patrick,steven]
%Y = sonia ? ;
%
%X = [natalie]
%Y = susanne ? ;
%
%X = [helen,andrew,john]
%Y = william
%
%yes
%| ?- nephews([natalie],susanne).
%
%yes
%| ?- nephews([patrick,steven],sonia).  
%
%yes





%  #8

allKids(F, M, Lst) :- bagof(Lst, (father(F, Lst), mother(M, Lst)), Lst).

family(Lst) :- nonvar(Lst), Lst = [F, M | T], allKids(F, M, Kids), permutation(Kids, T), !. 
family(Lst) :- var(Lst), Lst = [F, M | T], allKids(F, M, T).


%| ?- family([thomas,susanne,john,helen,andrew]).
%
%yes
%| ?- family([thomas,susanne,john,andrew,helen]).
%
%yes


%| ?- family(X).                                 
%
%X = [albert,sarah,susanne,william] ? ;
%
%X = [alex,rosa,steven] ? ;
%
%X = [donald,sarah,jane] ? ;
%
%X = [eugene,rachel,victor,rebecca] ? ;
%
%X = [harry,laura,lisa] ? ;
%
%X = [henry,laura,lucy,lily] ? ;
%
%X = [jack,mary,thomas] ? ;
%
%X = [max,miranda,nina,george] ? ;
%
%X = [michael,lisa,patrick] ? ;
%
%X = [patrick,lynn,max,robert,eugene] ? ;
%
%X = [paul,lynn,vivian] ? ;
%
%X = [peter,amelia,mary] ? ;
%
%X = [simon,jennifer,alex,sonia,michael] ? ;
%
%X = [thomas,julie,jennifer] ? ;
%
%X = [thomas,susanne,helen,andrew,john] ? ;
%
%X = [tony,miranda,monica] ? ;
%
%X = [william,ann,natalie]
%
%yes




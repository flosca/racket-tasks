myMax(X,Y,Z) :- 
     X >= Y, !, Z = X;
     X <  Y, Z = Y.


myMaxCool(X,Y,X) :- X >= Y, !.
myMaxCool(_,Y,Y).

% Проверка на простоту
myCompound(1) :- !, fail.
myCompound(N) :- N mod 2 =:= 0, !.
myCompound(N) :- N mod 3 =:= 0, !.
myCompound(N) :- N mod 5 =:= 0, !.
myCompound(N) :- write('lllll'), fail.







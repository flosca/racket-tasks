p1(X,Y,Z) :- Z=X+Y.
p2(X,Y,Z) :- Z is X+Y.

%  #1
factor(0, 1).
factor(N, Res) :-
  N > 0,
  N1 is N - 1,
  factor(N1, Res1),
  Res is N * Res1.


%  #2
even(N) :- N mod 2 =:= 0.

odd(N)  :- N mod 2 =\= 0.

factor2(0, 1).
factor2(1, 1).
factor2(N, Res) :-
    N > 1,
    N2 is N - 2,
    factor2(N2, Res1),
    Res is N * Res1.



%  #3
sir(N, Res) :- 
    even(N), !,
    Res is N // 2.    
sir(N, Res) :- 
    odd(N),
    Res is (3 * N + 1).

sirakuz(0, _).
sirakuz(N, A0) :-
    sir(A0, Res),
    write(Res),
    nl,
    N1 is N - 1,
    sirakuz(N1, Res).

%% 

% #4

isDivisor(N, Res) :- N mod Res =:= 0.
isDivisor(N, Res) :- Res * Res < N,
                     Acc is Res + 2,
                     isDivisor(N, Acc).


isPrime(2).
isPrime(3).
isPrime(N):- 
             N > 3 ,
             odd(N),
             \+isDivisor(N, 3).








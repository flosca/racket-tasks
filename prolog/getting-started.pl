%  #1

factorial(0, 1).
factorial(1, 1).
factorial(N, Res) :-
	N > 0,
	N1 is N - 1,
	factorial(N1, Res1),
	Res is N * Res1.

%  #2
oddDouble(1, 1).
oddDouble(N, Res) :-
    N > 0,
	N1 is N - 2,
	oddDouble(N1, Res1),
	Res is N * Res1.

evenDouble(0, 1).
evenDouble(N, Res) :-
    N > 0,
	N1 is N - 2,
	evenDouble(N1, Res1),
	Res is N * Res1.



doubleFactorial(N, Res) :-
      N mod 2 =:= 1,
      oddDouble(N, Res).

doubleFactorial(N, Res) :-
      N mod 2 =:= 0,
      evenDouble(N, Res).

%  #3
even(0).
even(N) :- N mod 2 =:= 0.

odd(0).
odd(N) :- N mod 2 =\= 0.


%  #4
isDivisor(N, Res) :- N mod Res =:= 0.
isDivisor(N, Res) :- Res * Res < N,
                     Acc is Res + 2,
                     isDivisor(N, Acc).


isPrime(2).
isPrime(3).
isPrime(N):- Res is 3,
             N > 3 ,
             odd(N),
             \+isDivisor(N, Res).


%  #5
sirakuz(N, Res) :- 
    0 is N mod 2,
    Res is (N / 2).

sirakuz(N, Res) :- 
    1 is N mod 2,
    Res is (3 * N + 1).
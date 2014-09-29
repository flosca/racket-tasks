% #1
% Напишите два предиката, принимающих список и элемент. 
% Первый предикат должен быть истинным, если заданный элемент является первым в списке.
% Второй предикат истинен, если заданный элемент является вторым в списке.


first([H|_], Elem) :-
    Elem is H.

second([_, H | _], Elem) :-
    Elem is H.

% #2
% Не используя стандартный предикат reverse, напишите предикат, принимающий
% два списка и истинный, если второй список есть обращение первого.


reverseAcc([], Lst, Lst).
reverseAcc([H|T], Res, Acc) :- 
    reverseAcc(T, Res, [H|Acc]).

reverse1(Lst, Res) :-
    reverseAcc(Lst, Res, []).


% #3
% Напишите предикат, принимающий истинные значения на симметричных списках.

symmetric(Lst) :-
    reverse1(Lst, Lst).

% #4
% Напишите предикат, принимающий два списка и истинный, если второй
% список получается из первого выкидыванием составных чисел.

odd(N)  :- N mod 2 =\= 0.

isDivisor(N, Res) :- N mod Res =:= 0.
isDivisor(N, Res) :- Res * Res < N,
                     Acc is Res + 2,
                     isDivisor(N, Acc).

isPrime(2).
isPrime(3).
isPrime(N) :- 
             N > 3 ,
             odd(N),
             \+isDivisor(N, 3).

removeCompounds([],[]) :- !.
removeCompounds([H | T], Res) :-
          isPrime(H),
          append(Res1,[H],Res),
          removeCompounds(T, Res1), !.
removeCompounds([H | T], Res) :-
          \+isPrime(H),
          append(Res1,[],Res),
          removeCompounds(T, Res1), !.

filterCompounds(Lst1, Lst2) :-
       removeCompounds(Lst1, Res),
       reverse(Res, Lst2).




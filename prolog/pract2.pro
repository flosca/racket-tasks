% #1

first([H | _], H).

second([_, H2 | _], H2).


%  #2

reverseAcc([], Lst, Lst).
reverseAcc([H|T], Res, Acc) :- 
    reverseAcc(T, Res, [H|Acc]).

reverse1(Lst, Res) :-
    reverseAcc(Lst, Res, []).


%  #3

symmetric(Lst) :- reverse1(Lst, Lst).


%  #4

removeCompounds([],[]) :- !.
removeCompounds([H | T], Res) :-
          isPrime(H),
          append([H],Res1,Res),
          removeCompounds(T, Res1), !.
removeCompounds([H | T], Res) :-
          \+isPrime(H),
          append([],Res1,Res),
          removeCompounds(T, Res1).


%   #5

prefix1(Lst1, Lst2) :-
    append(Lst1, _, Lst2).


contains(SubLst, Lst) :-
    prefix1(SubLst, Lst), !.
contains(SubLst, [_ | T]) :-
    contains(SubLst, T).


%  #6

mergeSort(Lst, [], Lst) :- !.
mergeSort([], Lst, Lst) :- !.
mergeSort([H1 | T1], [H2 | T2], [H1|T] ) :-
   H1 =< H2,
   merge(T1, [H2|T2], T), !.
mergeSort([H1 | T1], [H2 | T2], [H2 | T] ) :- 
   merge([H1|T1],   T2,  T).

%  #7

numlst(N, Lst) :-
    N =< 10,
    append([],[N],Lst), !.
numlst(N, Lst) :-
    N > 10,
    N1 is N rem 10,
    N2 is N // 10,
    numlst(N2, Lst1),
    append(Lst1, [N1], Lst).



%   #8
sublistSort(Elem, [], [], []) :- !.
sublistSort(Elem, [H | T], [H | L], R) :- 
  H =< Elem,
    sublistSort(Elem, T, L, R), !.
sublistSort(Elem, [H | T], L, [H | R]) :- 
    sublistSort(Elem, T, L, R).


quickSort([], []) :- !.
quickSort([H | T], Res) :- 
  sublistSort(H, T, L, R),
    quickSort(L, SortedLeft),
    quickSort(R, SortedRight),
    append(SortedLeft, [H], Res1),
    append(Res1, SortedRight, Res).











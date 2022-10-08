%===1===
search(X,[X|_]).
search(X, [_|Xs ]) :- search (X,Xs).

search_two(X, [X, _, X | _]).
search_two(X, [_|Xs]) :- search_two(X,Xs).
%Yes, it is fully relational. For example search_two(X, [b,c,a,d,a,d,e]). and search_two(a, X). can generate results

search_anytwo(X, [X | T]) :- search(X, T).
search_anytwo(X, [_|Xs]) :- search_anytwo(X,Xs).


%===2===
%decimal version
sizeD([], 0).
sizeD([_|Xs],N) :- sizeD(Xs ,N2), N is N2 + 1.
%No, it is not allow fully relational behaviour,  sizeD(X, 2). fails

size([], zero).
size([_|T], s(N)) :- size(T, N). 
%sizeD(X, s(s(zero))). works, and return X/[_, _] 

sum([], 0).
sum([H | T], S) :- sum(T, N), S is N + H.

average(L, A) :- average(L, 0, 0, A).
average([], C, S, A) :- A is S/C.
average([X|Xs], C, S, A) :-
	C2 is C+1,
	S2 is S+X,
	average(Xs, C2, S2, A).


max([H|T], Max) :- max(T, Max, H).
max([], Max, TempMax) :- Max is TempMax.
max([X|Xs], Max, TempMax) :-
	X > TempMax, 
	TempMax2 is X,
	max(Xs, Max, TempMax2).

max([X|Xs], Max, TempMax) :-
	X =< TempMax,
	max(Xs, Max, TempMax).


maxmin([H|T], Max, Min) :- maxmin(T, Max, Min, H, H).
maxmin([], Max, Min, TmpMax, TmpMin) :- 
	Max is TmpMax,
	Min is TmpMin.

maxmin([X|Xs], Max, Min, TmpMax, TmpMin) :-
	X > TmpMax, 
	TmpMax2 is X,
	maxmin(Xs, Max, Min, TmpMax2, TmpMin).

maxmin([X|Xs], Max, Min, TmpMax, TmpMin) :-
	X < TmpMin, 
	TmpMin2 is X,
	maxmin(Xs, Max, Min, TmpMax, TmpMin2).

maxmin([X|Xs], Max, Min, TmpMax, TmpMin) :-
	X >= TmpMin, 
	X =< TmpMax,
	maxmin(Xs, Max, Min, TmpMax, TmpMin).
	
%===3===
same([], []).
same([X|Xs], [X|Ys]) :- same(Xs, Ys).
% Yes, same has fully relational behaviour -> same(X, X). , same([a,b], X)., same(X, [a,b]). and same(X, Y). works

all_bigger([], []).
all_bigger([H1|T1], [H2|T2]) :- H1 > H2, all_bigger(T1, T2).

sublist([], L).
sublist([H|T], L) :- search(H, L), sublist(T, L). 

%===4===
seq(0, []).
seq(N, [0|T]) :- N2 is N - 1, seq(N2, T).

seqR(0, [0]).
seqR(N, [N | T]) :- N2 is N - 1, seqR(N2, T).


seqR2(N,  L) :- seqR2(N, N, L).
seqR2(0, Last, [Last]).
seqR2(N, Last, [V|List]) :- 
	N2 is N - 1,
	V is Last - N,
	seqR2(N2, Last, List).

%===5===

last([E], E).
last([H|T], E) :- last(T, E).
%last([1,2,3], X) -> X/3
%last([1,2,3], 5) -> no
%last([1,2,3], 3) -> yes

map([], []).
map([V2 | T2], [V | T]) :- V2 is V + 1, map(T2, T).  
%map(X, [1,3,2]). -> X/[2,4,3]
%map([2,2], [1,1]). -> yes
%map([2,2], [1,2]). -> no


filter([], []).
filter([H|T], [H|T2]) :- H>0, filter(T, T2).
filter([H|T], T2) :- H =< 0, filter(T, T2).
%filter([1,-10,3,1,-7], X). -> X/[1,3,1]

count([], 0).
count([H|T], V) :- 
	H > 0, 
	count(T, N),
	V is N + 1.
count([H|T], N) :-
	H =< 0,
	count(T, N). 
%count([3,-1,8,-7,1], K).-> K/3

dropRight([], N, []).
dropRight(L, N, []) :- sizeD(L,Size), N >= Size.
dropRight([H|T], N, [H|T2]) :-
	sizeD(T,Size), 
	Size >= N,
	dropRight(T, N, T2).
%dropRight([a,b,c,d], 5, X). -> X/[]
%dropRight([a,b,c,d], 4, X). -> X/[]
%dropRight([a,b,c,d], 2, X). -> X/[a,b]

%This solution do not use size but is not tail recursion
dropRight2(L, N, R) :- dropRight2(L, N, R, C).
dropRight2([], N, [],  0).
dropRight2([H|T], N, R, C2) :-  dropRight2(T, N, R, C), C2 is C + 1, C2 =< N.
dropRight2([H|T], N, [H|T2], C2) :- dropRight2(T, N, T2, C), C2 is C + 1, C2 > N.

%best solution (use only one size function and is a tail recursion)
dropRight3(L, N, R) :- sizeD(L, Size), dropRight3(L, N, R, Size).
dropRight3([H|T], N, [H| T2], Size) :- Size > N, Size2 is Size - 1, dropRight3(T, N, T2, Size2).
dropRight3(L, N, [], Size) :- Size =< N.


dropWhile([], []).
dropWhile([H|T], [H|T]) :- H =< 0.
dropWhile([H|T], T2) :- H > 0, dropWhile(T, T2).
%dropWhile([1,2,3], X). -> X/[]
%dropWhile([1,2,3,0], X).  -> X/[0]
%dropWhile([1,2,3,0,1], X). -> X/[0,1]
%dropWhile([0,1], X).  -> X/[0,1]

partition([], [], []).
partition([], A, B).
partition([H|T], [H|A], B) :- H > 0, partition(T,A,B).
partition([H|T], A, [H|B]) :- H =< 0, partition(T,A,B).
%partition([-1,1,0,2,-2], A, B). -> A/[1,2] , B/[-1,0,-2]

partition2([], [], []).
partition2([H|T], [], [H|T]) :- H > 0.
partition2([H|T], [H | T2], P2) :- H =< 0, partition2(T, T2, P2).


reversed(L, R) :- reversed(L, [], R).
reversed([H|T], A, R) :- reversed(T, [H|A], R).
reversed([], A, A).
%reversed([1,2,3], X). -> X/[3,2,1]
%reverse(X, [3,2,1]). -> X/[1,2,3]


drop([], N, []).
drop(L, 0, L).
drop([H|T], N, R) :- N2 is N - 1, drop(T, N2, R).
%drop([1,2,3,4,5], 3, X). -> X/[4,5]
%drop([1,2,3,4,5], X, [3,4,5]). -> X/2

take(L, 0, []).
take([], N, []).
take([H|T], N, [H|T2]) :- N2 is N-1, take(T, N2, T2).
%take([1,2,3,4,5], 3, X). ->  X/[1,2,3]
%take([1,2,3,4,5], X, [1,2]). -> X/2


zip([], [], []).
zip([H|T], [H2|T2], [(H,H2)|T3]) :- zip(T, T2, T3).
%zip([1,2,3], [a,b,c], X). -> X/[(1,a),(2,b),(3,c)]
%zip(X, Y, [(1,a),(2,b),(3,c)]). -> X/[1,2,3], Y/[a,b,c]
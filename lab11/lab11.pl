dropFirst(X, [X|T], T) :- !.
dropFirst(X, [H|T], [H|T2]) :- dropFirst(X, T, T2). 
%dropFirst(10, [20, 10, 30,10,40], X) -> X / [20,30,10,40]

dropLast(X, [X | T], [X | T2]) :- dropLast(X, T, T2), !.
dropLast(X, [X|T], T).
dropLast(X, [H | T], [H | T2]) :- dropLast(X, T , T2).
%dropLast(10, [20, 10, 30,10,40], X) ->  X / [20,10,30,40]

dropLast2(E, [], []).
dropLast2(E, [H|T], [H|T2]) :- member(E, T), !,  dropLast2(E, T, T2). 
dropLast2(E, [H|T], T).


dropAll(X, [X|T], T2) :- dropAll(X, T, T2), !.
dropAll(X, [], []).
dropAll(X, [H|T], [H|T2]) :- dropAll(X, T, T2).
%dropAll(10, [20, 10, 30,10,40], X) -> X / [20,30,40]


fromCircList([H|T], G) :- fromCircList([H|T], G, H).
fromCircList([L], [e(L,F)], F).
fromCircList([H1, H2|T], [e(H1, H2)| T2], F) :- fromCircList([H2| T] , T2, F).
%fromCircList([1,2,3], X). -> X/[e(1,2),e(2,3),e(3,1)]
%fromCircList([1], X). -> X/[e(1,1)]


% in_degree(+Graph , +Node , -Deg)
% Deg is the number of edges leading into Node
in_degree([], N, 0).
in_degree([e(_,N)| T], N, C2) :- in_degree(T, N, C), C2 is C + 1, !.
in_degree([e(_,X)| T], N, C) :- in_degree(T, N, C), !.
%in_degree([e(1,2), e(1,3), e(3,2)], 1, X). -> X/0
%in_degree([e(1,2), e(1,3), e(3,2)], 2, X). -> X/2

dropNode([], N, []).
dropNode([e(N,_) | T], N, L) :- dropNode(T, N, L), !.
dropNode([e(_,N) | T], N, L) :-dropNode(T, N, L), !.
dropNode([H|T], N, [H|T2]) :- dropNode(T, N, T2).
%dropNode([e(1,2),e(1,3),e(2,3)],1, X). -> X / [e(2,3)]
%can be used the function dropAll


reaching([], N, []).
reaching([e(N, B)| T], N, [B|T2]) :- reaching(T, N, T2), !.
reaching([e(A,B)| T], N, L) :- reaching(T, N, L), !.
%reaching([e(1,2),e(1,3),e(2,3)],1,L). -> L/[2,3]
%reaching([e(1,2),e(1,2),e(2,3)],1,L). -> L/[2,2])

reaching2(G, N, L) :- findall(B,  (member(e(A,B), G), A = N), L).

% a path from Node1 to Node2
% if there are many path , they are showed 1-by -1
anypath([e(FN,LN)| T], FN, LN, [e(FN,LN)]).
anypath([e(FN, B) |T], FN, LN, [e(FN,B)| L]) :- anypath(T, B, LN, L).
anypath([e(A,_)|T], FN, LN, L) :- anypath(T, FN, LN, L).  
%anypath([e(1,2),e(1,3),e(2,3)],1,3,L). 
% -> L / [e(1,2),e(2,3)]
% -> L / [e(1,3)]

%all the nodes that can be reached from Node
%Suppose the graph is NOT circular!
allreaching(G, N, L) :- findall(K,  (anypath(G, N, K, R)), L).
%allreaching([e(1,2),e(2,3),e(3,5)],1, X). -> X / [2,3,5]

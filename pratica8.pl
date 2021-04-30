% 1---------------------------------------------------------------------
% insere_ordenado(X,L1,L2)

% recursivo
% insere_odenado(_,[],L2).
% insere_ordenado(X,[P|R],[X,P|R]) :- X < P.
% insere_ordenado(X,[P|R],[P|RES]) :- X >= P, insere_ordenado(X,R,RES).

% usando meta predicados
insere_ordenado(E,L1,L2) :- 
    findall(X,(member(X,L1),X < E), Menores),
    findall(X,(member(X,L1),X > E), Maiores),
    append(Menores,[E|Maiores], L2).

% 2----------------------------------------------------------------------
% junta_novo_aleatorio(L1,LI,LS,L2)

% usando meta predicados

junta_novo_aleatorio(L1,LI,LS,L2) :-
    random_between(LI,LS,N),
    \+ member(N,L1), % n membro
    insere_ordenado(N,L1,L2).

% 3----------------------------------------------------------------------
% repete_el(E,N,L)

% recursivamente

% repete_el(_,0,[]).

% repete_el(E,N,[E|R]) :-
%    NEW_N is N-1,
%    repete_el(E,NEW_N,R).

% usando meta predicados

repete_el(E,N,L) :- 
    length(L,N),
    maplist(=(E),L).

% 4----------------------------------------------------------------------
% duplica_elementos(L1,L2)

% recursivamente

% duplica_elementos([],[]).

% duplica_elementos([P1|R1],[P1,P1|R2]):-
%     duplica_elementos(R1,R2).

% usando meta predicados

duplica_elementos(L1,L2):-
    findall([E,E],member(E,L1),AUX),
    append(AUX,L2).

% 5----------------------------------------------------------------------
% num_occ(L,E,N)

% usando meta predicados

num_occ(L,E,N) :-
    findall(X,(member(X,L), X == E), AUX),
    length(AUX,N).

% 6----------------------------------------------------------------------
% substitui_maiores_N(N,S,L1,L2)

% recursivamente

% substitui_maiores_N(_,_,[],[]).

% substitui_maiores_N(N,S,[P|R1],[S|R2]):-
%    N < P,
%    substitui_maiores_N(N,S,R1,R2).

% substitui_maiores_N(N,S,[P|R1],[P|R2]):-
%    N >= P,
%    substitui_maiores_N(N,S,R1,R2).

% utilizando predicados

% substitui_maiores_N(N,S,L1,L2):-
%    maplist(substitui_aux(N,S),L1,L2).

% qa
% lista_maiores(N,L1,L2)

lista_maiores(N,L1,L2):-
    findall(X,(member(X,L1), X>N),L2).

%----------------------------------------------------------------=

pertence(E,[Q|_]) :- E == Q.

pertence(E,[_|R]):- pertence(E,R).
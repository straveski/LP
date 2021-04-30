%----------------------------
%         PROJETO LP
%----------------------------
% realizado por: Diogo Falcao
%----------------------------

%-------------------------------------------------------------------------------
% combinacao(N, Els, Comb)
% combinacao(N, Els, Comb) significa que Comb eh uma
% combinacao dos elementos de Els, N a N
%-------------------------------------------------------------------------------
combinacao(0,_,[]).
combinacao(N,[X|T], [X|Comb]):-
    N > 0,
    N1 is N-1,
    combinacao(N1, T, Comb).
combinacao(N, [_|T], Comb):-
    N > 0,
    combinacao(N, T, Comb).

% CARREGAR CODIGO COMUM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

%-----------------------------------
% 3.1.1 Predicado combinacoes_soma/4
%-----------------------------------

combinacoes_soma(N,Els,Soma,Combs):- findall(Comb,(combinacao(N,Els,Comb), sum_list(Comb,Soma)),Combs).


%-----------------------------------
% 3.1.2 Predicado permutacoes_soma/4
%-----------------------------------
permutacoes_comb(Comb,Perms):- findall(Perm,permutation(Comb,Perm),Perms).

permutacoes_soma(N,Els,Soma,Perms):- 
    combinacoes_soma(N,Els,Soma,Combs),
    maplist(permutacoes_comb,Combs,Temp),
    append(Temp,Temp2),
    sort(Temp2,Perms).

%------------------------------------
% 3.1.3 Predicado espaco_fila/2
%------------------------------------

espaco_fila(Fila,Esp,H_V):-
    





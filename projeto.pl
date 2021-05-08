%----------------------------
%         PROJETO LP
%----------------------------
% realizado por: Diogo Falcao
%----------------------------

% ficheiro codigo_comum
:- [codigo_comum].

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
eh_pref(Pref):-
    last(Pref,X),
    \+ var(X).

eh_suf(Suf):-
    (nth1(1,Suf,X), \+ var(X)) 
    ; 
    Suf == [].

espaco_fila(Fila,espaco(S,Esp),H_V):-
    H_V == h,
    append([Pref,Esp,Suf],Fila),
    Esp \== [],
    eh_pref(Pref),
    eh_suf(Suf),
    maplist(var,Esp),
    last(Pref,Aux),
    nth1(2,Aux,S).

espaco_fila(Fila,espaco(S,Esp),H_V):-
    H_V == v,
    append([Pref,Esp,Suf],Fila),
    Esp \== [],
    eh_pref(Pref),
    eh_suf(Suf),
    maplist(var,Esp),
    last(Pref,Aux),
    nth1(1,Aux,S).

%------------------------------------
% 3.1.4 Predicado espacos_fila/2
%------------------------------------

espacos_fila(H_V,Fila,Esps):-
    findall(Esp,espaco_fila(Fila,Esp,H_V),Esps).
%------------------------------------
% 3.1.5 Predicado espacos_puzzle/2
%------------------------------------
    




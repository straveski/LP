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
    bagof(Esp,espaco_fila(Fila,Esp,H_V),Esps),!; Esps = [].

espacos_fila_aux(H_V,Fila,Esps):-
    bagof(Esp,espaco_fila(Fila,Esp,H_V),Esps).
%------------------------------------
% 3.1.5 Predicado espacos_puzzle/2
%------------------------------------

membro(E, [E|_]).
membro(E, [_|R]):- membro(E, R).

espacos_puzzle(Puzzle, Espacos):-
    %Encontra os espacos horizontais
    bagof(Esps,X^(membro(X,Puzzle), espacos_fila_aux(h,X,Esps)),Aux1),
    mat_transposta(Puzzle,Trans),
    % com a transposta, vai encontrar os espacos verticais
    bagof(Esps,X^(membro(X,Trans), espacos_fila_aux(v,X,Esps)),Aux2),
    % vai juntar todos os espacos
    append(Aux1,Aux2,Aux),
    flatten(Aux,Espacos).
    
%-----------------------------------------------
% 3.1.6 Predicado espacos_com_posicoes_comuns/3
%-----------------------------------------------
soma(espaco(S,_),S).
esp_vars(espaco(_,L),L).

membro1(E, [P|_]):- P == E.
membro1(E, [_|R]):- membro1(E, R).
% prof usou include e uma funcao aux

same(X,Y):-
    esp_vars(Y,Aux),
    membro1(X,Aux).

espacos_com_posicoes_comuns(Espacos,Esp,Esps_com):-
    % listas das posicoes do espacos que eu quero procurar
    esp_vars(Esp,Aux1),
    % vai procurar em todas as posicoes dessa lista todos os espacos 
    % em que aparecem na lista de todos os espacos 
    % em que essas posicoes aparecem
    bagof(Sub_list,X^(member(X,Aux1),include(same(X),Espacos,Sub_list)),Subs),
    flatten(Subs,Subs2),
    % excluir o espaco em que estamos a procurar
    subtract(Subs2,[Esp],Esps_com).

%-----------------------------------------------
% 3.1.7 Predicado permutacoes_soma_espacos/2
%-----------------------------------------------

permutacoes_soma_espacos_aux(X, Perms_soma):-
    esp_vars(X,Vars),
    length(Vars,Quant),
    soma(X,S),
    permutacoes_soma(Quant,[1,2,3,4,5,6,7,8,9],S,Perms_soma).
        

permutacoes_soma_espacos(Espacos, Perms_soma):-
    bagof(Aux1,X^(member(X,Espacos),permutacoes_soma_espacos_aux(X,Aux),append([X],[Aux],Aux1)),Perms_soma).

    
    





%-----------------------------------------------
% 3.1.11 Predicado numeros_comuns/2
%-----------------------------------------------
% usar forall

numeros_comuns(Lst_Perms, Numeros_comuns):-
    numeros_comuns(Lst_Perms, Numeros_comuns,1).

numeros_comuns(Lst_Perms,Numeros_comuns,Comp):-
    nth1(1,Lst_Perms,Lst_aux),
    (Comp =< length(Lst_aux,Max_comp)) ->
        nth1(Comp,Lst_aux,Term),
        forall(member(X,Lst_Perms),(nth1(Comp,X,Aux),Term == Aux)),
        append([(Comp,X)],[],New_Numeros_comuns),
        New_comp is Comp + 1,
        numeros_comuns(Lst_perms,New_Numeros_comuns,New_comp)
    ;
    numeros_comuns(_,[],_).
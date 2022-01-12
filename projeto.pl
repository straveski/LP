%----------------------------
%         PROJETO LP
% realizado por: Diogo Falcao
%----------------------------
% ficheiros
:- [codigo_comum].
:- [puzzles_publicos].
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
    % Encontra os espacos horizontais
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

same(X,Y):-
    esp_vars(Y,Aux),
    membro1(X,Aux).

% elimina sem unificar as variaveis
elimina(_, [], []):- !.
elimina(X, [X1|Xs], Ys) :- 
    X == X1,
    !,
    elimina(X, Xs, Ys).
elimina(X, [X1|Xs], [X1|Ys]) :- 
    X \== X1, 
    elimina(X, Xs, Ys).

espacos_com_posicoes_comuns(Espacos,Esp,Esps_com):-
    % listas das posicoes do espacos que eu quero procurar
    esp_vars(Esp,Aux1),
    % vai procurar em todas as posicoes dessa lista todos os espacos 
    % em que aparecem na lista de todos os espacos 
    % em que essas posicoes aparecem
    bagof(Sub_list,X^(member(X,Aux1),include(same(X),Espacos,Sub_list)),Subs),
    flatten(Subs,Subs2),
    elimina(Esp,Subs2,Esps_com).
    % excluir o espaco em que estamos a procurar

%-----------------------------------------------
% 3.1.7 Predicado permutacoes_soma_espacos/2
%-----------------------------------------------

permutacoes_soma_espacos_aux(X, Perms_soma):-
    esp_vars(X,Vars),
    length(Vars,Quant),
    soma(X,S),
    permutacoes_soma(Quant,[1,2,3,4,5,6,7,8,9],S,Perms_soma).
        

permutacoes_soma_espacos(Espacos, Perms_soma):-
    bagof(Aux,X^(member(X,Espacos),permutacoes_soma_espacos_aux(X,Aux)),Perms_soma_aux),
    adiciona_espacos(Espacos,Perms_soma_aux,Perms_soma).

adiciona_espacos([],[],[]).

adiciona_espacos([P1|R1],[P2|R2],[[P1,P2]|R3]):-
    adiciona_espacos(R1,R2,R3).

%-----------------------------------------------
% 3.1.8 Predicado permutacao_possivel_espaco/4
%-----------------------------------------------

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-
    espacos_com_posicoes_comuns(Espacos,Esp,Esps_com),
    % encontra as permutacoes deste espaco
    encontra_perms_espaco(Esp,Perms_soma,Perms_esp_1),
    % X e uma permutacao
    findall(X,(member(X,Perms_esp_1),existe_intersec(X,Esps_com,Perms_soma)),Perm_aux),
    member(Perm,Perm_aux).

% para cada numero da perm existe uma intersec com a linha ou coluna certa
existe_intersec(Perm,Esps_com,Perms_soma):-
    forall(member(X,Perm),(nth1(Ind,Perm,X),nth1(Ind,Esps_com,Intersec),encontra_perms_espaco(Intersec,Perms_soma,Perms_esp),existe_intersec_numero(X,Perms_esp),!)).

% numero esta em qualquer perm
existe_intersec_numero(Num,Perms_esp):- flatten(Perms_esp,Aux), membro1(Num,Aux).

% Esta funcao encontra as permutacoes de um espaco
encontra_perms_espaco(Esp,Perms_soma,Perms_esp):-
    findall(Perms_aux,(member(X,Perms_soma),nth1(1,X,Esp),!,nth1(2,X,Perms_aux)),Perms_esp_aux),
    Perms_esp_aux \== [] -> append(Perms_esp_aux,Perms_esp);Perms_esp = [].


%-----------------------------------------------
% 3.1.9 Predicado permutacoes_possiveis_espaco/4
%-----------------------------------------------
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss):-
    espacos_com_posicoes_comuns(Espacos,Esp,Esps_com),
    % encontra as permutacoes deste espaco
    encontra_perms_espaco(Esp,Perms_soma,Perms_esp_1),
    % X e uma permutacao
    findall(X,(member(X,Perms_esp_1),existe_intersec(X,Esps_com,Perms_soma)),Perm_aux),
    esp_vars(Esp,Vars),
    append([Vars],[Perm_aux],Perms_poss).


%-------------------------------------------------
% 3.1.10 Predicado permutacoes_possiveis_espacos/2
%-------------------------------------------------
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perms_poss,X^(member(X,Espacos),
    permutacoes_possiveis_espaco(Espacos, Perms_soma,X,Perms_poss)),
    Perms_poss_esps),!.

%-----------------------------------------------
% 3.1.11 Predicado numeros_comuns/2
%-----------------------------------------------
numeros_comuns(Lst_Perms, Numeros_comuns):-
    nth1(1,Lst_Perms,P_perm),
    length(P_perm,Comp),
    numlist(1,Comp,L_indices),
    bagof(X,X^Y^(member(X,L_indices),forall(member(L,Lst_Perms),(nth1(X,P_perm,Aux),nth1(X,L,Y),Aux == Y))),Indices_comuns),
    bagof(Y,X^(member(X,Indices_comuns),nth1(X,P_perm,Y)),Num_com),
    junta_pares(Indices_comuns,Num_com,Numeros_comuns),!;Numeros_comuns = [].


junta_pares([],[],[]).
junta_pares([P1|R1],[P2|R2],[(P1,P2)|R3]):-
    junta_pares(R1,R2,R3).

%-----------------------------------------------
% 3.1.12    Predicado atribui_comuns/1
%-----------------------------------------------
atribui_comuns(Perms_Possiveis):-
    maplist(unifica,Perms_Possiveis).

unifica(X):-
    nth1(2,X,Perms),
    numeros_comuns(Perms,Comuns),
    nth1(1,X,Vars),
    Comuns \== []-> maplist(unifica_var(Vars),Comuns);
    !.

unifica_var(Y,(Ind,Number)):-
    nth1(Ind,Y,Number).
    
%-----------------------------------------------
% 3.1.13 Predicado retira_impossiveis/2
%-----------------------------------------------
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis):-
    % percorremos as perm_possiveis e vamos a cada uma delas e verificamos se tem ou nao 
    % ja unificacao e eliminamos as permutacoes que tenham um numero diferente nessa posicao
    bagof(Possivel,X^(member(X,Perms_Possiveis),
    retira_imp_aux2(X,Possivel)),
    Novas_Perms_Possiveis).

retira_imp_aux2(X,Possivel):-
    nth1(1,X,Vars),
    nth1(2,X,Perms),
    encontra_uni(Vars,Posicoes),
    retira_imp_aux(Perms,Posicoes,S_pos),
    append([Vars],[S_pos],Possivel).

encontra_uni(Vars,Posicoes):-
    encontra_uni(Vars,Posicoes,1).
encontra_uni([],[],_).
encontra_uni([P1|R1],[(Aux,P1)|R2],Aux):-
    \+var(P1),
    !,
    New_aux is Aux+1,
    encontra_uni(R1,R2,New_aux).
encontra_uni([_|R1],R2,Aux):-
    New_aux is Aux+1,
    encontra_uni(R1,R2,New_aux).

retira_imp_aux(Perms,Posicoes,S_pos):-
    include(inclui_pos(Posicoes),Perms,S_pos).

inclui_pos(Posicoes,X):-
    forall(member((I,N),Posicoes),nth1(I,X,N)).

%-----------------------------------------------
% 3.1.14 Predicado simplifica/2
%-----------------------------------------------
simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Aux1),
    Aux1 \== Perms_Possiveis,
    !,
    simplifica(Aux1,Novas_Perms_Possiveis). 
    
simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Aux1),
    Novas_Perms_Possiveis = Aux1.

%-----------------------------------------------
% 3.1.15 Predicado inicializa/2
%----------------------------------------------- 
inicializa(Puzzle, Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Aux),
    simplifica(Aux, Perms_Possiveis).

%-----------------------------------------------
% 3.2.1 Predicado escolhe_menos_alternativas/2
%----------------------------------------------- 

% fazer lista de tamanhos das perms
% ver qual o menor sem ser 1
% achar o indice
% retornar
% caso de ser false, ou seja, lista de legths 1's

escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
    findall(Comps,(member(X,Perms_Possiveis),nth1(2,X,Perms),length(Perms,Comps)),Escolha),
    forall(member(X,Escolha),X\==1).

escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
    findall(Comps,(member(X,Perms_Possiveis),nth1(2,X,Perms),length(Perms,Comps)),L_comps),
    exclude(=(1),L_comps,L_comps_s_1),
    min_list(L_comps_s_1,Min),
    nth1(Ind,L_comps_s_1,Min),
    !,
    nth1(Ind,Perms_Possiveis,Escolha).

%-----------------------------------------------
% 3.2.2 Predicado experimenta_perm/3
%----------------------------------------------- 
experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis):-
    nth1(1,Escolha,Esp),
    nth1(2,Escolha,Lst_Perms),
    member(Perm,Lst_Perms),
    !,
    Esp = Perm,
    select(Escolha,Perms_Possiveis,[Esp, [Perm]],Novas_Perms_Possiveis).

%-----------------------------------------------
% 3.2.3 Predicado resolve_aux/2
%----------------------------------------------- 
% forall(member(X,Aux),(nth1(2,X,Perms),length(Perms,1))),
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis,Aux),
    % verificar se caso de paragem (quando so ha uma perm pos)
    findall(Comp,(member(X,Aux),nth1(2,X,Perms),length(Perms,Comp)),L_comps),
    exclude(=(1),L_comps,L_comps_s_1),
    length(L_comps_s_1,Comp_aux),
    Comp_aux >= 1,
    !,
    simplifica(Aux,Aux1),
    resolve_aux(Aux1, Novas_Perms_Possiveis).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis,Aux1),
    simplifica(Aux1, Aux),
    Novas_Perms_Possiveis = Aux.

%-----------------------------------------------
% 3.3.1 Predicado resolve/1
%----------------------------------------------- 
resolve(Puz):-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis,Puz).

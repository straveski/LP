%----------------------------
%         PROJETO LP
%----------------------------
% realizado por: Diogo Falcao
%----------------------------

%ficheiro codigo_comum
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

%eh_lista(L):-
%    lenght() == 2
%    L != [0,0]

sublistas([P|R],Sub):-
    \+ var(P) -> (Aux = P, sublistas(R,Sub))
    ;
    append([P],Aux,Sub), sublistas(R,Sub).


sub_espacos(Fila,H_V,espaco(Soma,L)):-
    H_V = h,
    sublistas(Fila,Sub),
    %calcula o comp da lista
    length(Sub,Ind1),
    Ind2 is Ind1-1,
    nth1(Ind1,Sub,Soma),
    nth1(Ind2,Sub,Aux),
    %vou eliminar da sublista os nuneros da soma para ficarem so as variaveis
    subtract(Sub,[Soma,Aux],L).

sub_espacos(Fila,H_V,espaco(Soma,L)):-
    H_V = v,
    sublistas(Fila,Sub),
    length(Sub,Ind1),
    Ind2 is Ind1-1,
    nth1(Ind2,Sub,Soma),
    nth1(Ind1,Sub,Aux),
    subtract(Sub,[Soma,Aux],L).

espaco_fila(Fila,Esp,H_V):-
    sub_espacos(Fila,H_V,Esp).
    
%------------------------------------
% 3.1.4 Predicado espacos_fila/2
%------------------------------------

espacos_fila(H_V,Fila,Espacos):-
    findall(Aux,espaco_fila(Fila,Aux,H_V),Espacos).

%------------------------------------
% 3.1.5 Predicado espacos_puzzle/2
%------------------------------------
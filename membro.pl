membro(E,[E | _]).

membro(E,[_|R]) :- membro(E,R).

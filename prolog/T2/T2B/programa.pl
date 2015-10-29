/*  Caique Rodrigues Marques
    Gustavo José Carpeggiani
    Vinícius Couto Biermann

   Programacao Logica - Prof. Alexandre G. Silva - UFSC
     Versao inicial     : 30set2015
     Adicao de gramatica: 15out2015
   
   RECOMENDACOES:
   
   - O nome deste arquivo deve ser 'programa.pl'
   - O nome do banco de dados deve ser 'desenhos.pl'
   - O nome do arquivo de gramatica deve ser 'gramatica.pl'
   
   - Dicas de uso podem ser obtidas na execucação: 
     ?- menu.
     
   - Exemplo de uso:
     ?- load.
     ?- searchAll(id1).

   - Exemplo de uso da gramatica:
     ?- comando([repita, '8', '[', pf, '50', gd, '45', ']'], []).
     Ou simplesmente:
     ?- cmd("repita 8[pf 50 gd 45]").
*/

:- consult('../T2A/programa.pl').
:- initialization(new0(0)).

% Coloca tartaruga no centro da tela (de 1000x1000)
% Implementacao incompleta:
%   - Considera apenas id1 e efetua new sem verificar sua existencia
%   - Supoe que ha' o xylast em 'desenhos.pl'
new0(Id) :-
    consult('gramatica.pl'),
    load,
    nb_setval(pencil, 1),
    (check_xy(Id) -> xylast(X, Y),
                    new(Id, X, Y),
                    retractall(xylast(Id, _, _)),
                    asserta(xylast(Id, X, Y));
            new_angle(Id, 90),
            new(Id, 500, 500),
            asserta(xylast(Id, 500, 500)),
            true).

% Checa se há xy no banco de dados.
check_xy(Id) :-
    xy(Id, _, _), !.

check_pencil(L) :-
    nb_getval(pencil, L).

% Cria uma nova posição com o ângulo em relação a X.
% A medição considerada do ângulo é em graus.
new_angle(Id, Angle) :-
    retractall(angle(Id, _)),
    asserta(angle(Id, Angle)).

% Limpa os desenhos e reinicia no centro da tela (de 1000x1000)
% Implementacao incompleta:
%   - Considera apenas id1
tartaruga :-
    retractall(xy(_,_,_)),
    retractall(xylast(_,_)),
    retractall(angle(_, _)),
    asserta(xylast(500, 500)),
    new0(0).

% Para frente N passos
%   Funções:
%       cos(Value)
%       Calcula o cosseno de Value, sendo Value o ângulo
%       em radianos.
%
%       sin(Value)
%       Calcula o seno de Value, sendo Value o ângulo
%       em radianos.
%
%       nb_getval(Value, Var)
%       Armazena o valor de Value em Var.
%
% Implementacao incompleta:
%   - Considera apenas id1
%   - Somando apenas em X, ou seja, nao considera a inclinacao da tartaruga
parafrente(Id, N) :-
    xylast(X, Y),
    angle(Id, Degree),
    Radian is ((Degree*pi)/(180)),
    Destination_X is N*sin(Radian)+X,
    Destination_Y is N*cos(Radian)+Y,
    nb_getval(pencil, Pencil),
        write('MOVIMENTAÇÃO PARA FRENTE'), nl,
        write('Posição do lápis: '), print(Pencil), nl,
        write('Posição atual (X, Y): '), print(xylast(X, Y)), nl,
        write('Ângulo atual (em graus): '), print(Degree), nl,
        write('Ângulo atual (em radianos): '), print(Radian), nl,
        write('Destino (ponto X): '), print(Destination_X), nl,
        write('Destino (ponto Y): '), print(Destination_Y), nl,
    (
        Pencil =:= 1 -> new(Id, Destination_X, Destination_Y),
                        retractall(xylast(_, _)),
                        asserta(xylast(Destination_X, Destination_Y)), !;
                        retractall(xylast(_, _)),
                        asserta(xylast(Destination_X, Destination_Y)), !
    ).
    %write('Revisar: pf '), writeln(N),
    %xylast(X, Y),
    %Xnovo is X + N,
    %new(Id, Xnovo, Y),
    %retractall(xylast(_,_)),
    %asserta(xylast(Xnovo, Y)).

% Para tras N passos
paratras(Id, N) :-
    xylast(X, Y),
    angle(Id, Degree),
    Radian is ((Degree*pi)/(180)),
    Destination_X is (N*sin(Radian))*(-1)+X,
    Destination_Y is (N*cos(Radian))*(-1)+Y,
    nb_getval(pencil, Pencil),
        write('MOVIMENTAÇÃO PARA TRÁS'), nl,
        write('Posição do lápis: '), print(Pencil), nl,
        write('Posição atual (X, Y): '), print(xylast(X, Y)), nl,
        write('Ângulo atual (em graus): '), print(Degree), nl,
        write('Ângulo atual (em radianos): '), print(Radian), nl,
        write('Destino (ponto X): '), print(Destination_X), nl,
        write('Destino (ponto Y): '), print(Destination_Y), nl,
    (
        Pencil =:= 1 -> new(Id, Destination_X, Destination_Y),
                        retractall(xylast(_, _)),
                        asserta(xylast(Destination_X, Destination_Y)), !;
                        retractall(xylast(_, _)),
                        asserta(xylast(Destination_X, Destination_Y)), !
    ).
    %write('Implementar: pt '), writeln(N).

% Gira a direita G graus
giradireita(G) :-
    angle(_, Degree),
    nb_getval(pencil, Pencil),
    New_degree is Degree - G,
    write('GIRANDO À DIREITA'), nl,
    write('Posição do lápis: '), print(Pencil), nl,
    write('Posicionamento anterior (ângulo em graus): '), print(Degree), nl,
    write('Posicionamento atual (ângulo em graus): '), print(New_degree), nl,
    new_angle(_, New_degree), !.
    %write('Implementar: gd '), writeln(G).

% Gira a esquerda G graus
giraesquerda(G) :-
    angle(_, Degree),
    nb_getval(pencil, Pencil),
    New_degree is Degree + G,
    write('GIRANDO À ESQUERDA'), nl,
    write('Posição do lápis: '), print(Pencil), nl,
    write('Posicionamento anterior (ângulo em graus): '), print(Degree), nl,
    write('Posicionamento atual (ângulo em graus): '), print(New_degree), nl,
    new_angle(_, New_degree), !.
    %write('Implementar: ge '), writeln(G).

% Use nada (levanta lápis)
%   Função:
%       nb_setval(Var, Value)
%       Armazena o valor de Value em Var.
usenada :-
    nb_setval(pencil, 0).

% Use lapis
uselapis :-
    nb_getval(pencil, Pencil),
    check_xy(Id),
    copy_term(Id, New_id),
    (
        Pencil =:= 0 -> Final_id is New_id+1,
                        new0(Final_id);
                nb_setval(pencil, 1)
    ).

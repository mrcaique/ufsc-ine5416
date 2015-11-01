/*  Caique Rodrigues Marques 13204303
    Gustavo José Carpeggiani 13103524
    Vinícius Couto Biermann  13100778

   Programação Lógica - Prof. Alexandre G. Silva - UFSC
     Versão inicial     : 30set2015
     Adição de gramática: 15out2015
   
   RECOMENDAÇÕES:
   
   - O nome deste arquivo deve ser 'programa.pl'
   - O nome do banco de dados deve ser 'desenhos.pl'
   - O nome do arquivo de gramática deve ser 'gramatica.pl'
   
   - Dicas de uso podem ser obtidas na execução: 
     ?- menu.
     
   - Exemplo de uso:
     ?- load.
     ?- searchAll(id1).

   - Exemplo de uso da gramática:
     ?- comando([repita, '8', '[', pf, '50', gd, '45', ']'], []).
     Ou simplesmente:
     ?- cmd("repita 8[pf 50 gd 45]").
*/

:- consult('../T2A/programa.pl').
:- initialization(new0(0)).

% Coloca tartaruga no centro da tela (de 1000x1000)
new0(Id) :-
    consult('gramatica.pl'),
    load,
    nb_setval(actual_id, Id),
    nb_setval(pencil, 1),
    (
        check_xy_last -> xylast(X, Y),
                        new(Id, X, Y),
                        retractall(xylast(_, _)),
                        asserta(xylast(X, Y));
                new_angle(90),
                new(Id, 500, 500),
                asserta(xylast(500, 500)),
                true
    ).

% Checa o ponto xy no banco de dados.
%
% Id = Identificador do desenho.
check_xy(Id) :-
    xy(Id, _, _), !.

% Checa o último ponto xy
check_xy_last :-
    xylast(_, _), !.

% Apresenta a atual posição do lápis, ou seja,
% se o lápis está no papel (1) ou não está (zero).
%
% L = Posição do lápis
check_pencil(L) :-
    nb_getval(pencil, L).

% Cria uma nova posição com o ângulo em relação ao eixo X.
% A medição considerada do ângulo é em graus.
%
% Angle = Novo ângulo para o desenho com identificador Id
new_angle(Angle) :-
    retractall(angle(_)),
    asserta(angle(Angle)).

% Limpa os desenhos e reinicia no centro da tela (de 1000x1000)
tartaruga :-
    retractall(xy(_,_,_)),
    retractall(xylast(_,_)),
    retractall(angle(_)),
    asserta(xylast(500, 500)),
    assertz(angle(90)),
    commit,
    new0(0), !.

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
parafrente(N) :-
    xylast(X, Y),
    nb_getval(actual_id, Id),
    angle(Degree),
    Radian is ((Degree*pi)/(180)),
    Destination_X is N*sin(Radian),
    Destination_Y is N*cos(Radian),
    New_xlast is Destination_X+X,
    New_ylast is Destination_Y+Y,
    nb_getval(pencil, Pencil),
        write('MOVIMENTAÇÃO PARA FRENTE'), nl,
        write('Id atual: '), print(Id), nl,
        write('Posição do lápis: '), print(Pencil), nl,
        write('Última posição (X, Y): '), print(xylast(X, Y)), nl,
        write('Ângulo atual (em graus): '), print(Degree), nl,
        write('Ângulo atual (em radianos): '), print(Radian), nl,
        write('Destino (ponto X): '), print(New_xlast), nl,
        write('Destino (ponto Y): '), print(New_ylast), nl, nl,
    (
        Pencil =:= 1 -> new(Id, Destination_X, Destination_Y),
                        retractall(xylast(_, _)),
                        asserta(xylast(New_xlast, New_ylast)), !;
                retractall(xylast(_, _)),
                asserta(xylast(New_xlast, New_ylast)), !
    ).

% Para tras N passos
paratras(N) :-
    xylast(X, Y),
    nb_getval(actual_id, Id),
    angle(Degree),
    Radian is ((Degree*pi)/(180)),
    Destination_X is (N*sin(Radian))*(-1),
    Destination_Y is (N*cos(Radian))*(-1),
    New_xlast is Destination_X+X,
    New_ylast is Destination_Y+Y,
    nb_getval(pencil, Pencil),
        write('MOVIMENTAÇÃO PARA TRÁS'), nl,
        write('Id atual: '), print(Id), nl,
        write('Posição do lápis: '), print(Pencil), nl,
        write('Última posição (X, Y): '), print(xylast(X, Y)), nl,
        write('Ângulo atual (em graus): '), print(Degree), nl,
        write('Ângulo atual (em radianos): '), print(Radian), nl,
        write('Destino (ponto X): '), print(New_xlast), nl,
        write('Destino (ponto Y): '), print(New_ylast), nl, nl,
    (
        Pencil =:= 1 -> new(Id, Destination_X, Destination_Y),
                        retractall(xylast(_, _)),
                        asserta(xylast(New_xlast, New_ylast)), !;
                retractall(xylast(_, _)),
                asserta(xylast(New_xlast, New_ylast)), !
    ).

% Gira a direita G graus
giradireita(G) :-
    angle(Degree),
    nb_getval(pencil, Pencil),
    New_degree is Degree - G,
    write('GIRANDO À DIREITA'), nl,
    write('Posição do lápis: '), print(Pencil), nl,
    write('Posicionamento anterior (ângulo em graus): '), print(Degree), nl,
    write('Posicionamento atual (ângulo em graus): '), print(New_degree), 
    nl, nl,
    new_angle(New_degree), !.

% Gira a esquerda G graus
giraesquerda(G) :-
    angle(Degree),
    nb_getval(pencil, Pencil),
    New_degree is Degree + G,
    write('GIRANDO À ESQUERDA'), nl,
    write('Posição do lápis: '), print(Pencil), nl,
    write('Posicionamento anterior (ângulo em graus): '), print(Degree), nl,
    write('Posicionamento atual (ângulo em graus): '), print(New_degree), 
    nl, nl,
    new_angle(New_degree), !.

% Use nada (levanta lápis)
%   Função:
%       nb_setval(Var, Value)
%       Armazena o valor de Value em Var.
usenada :-
    nb_setval(pencil, 0).

% Use lápis
%   Função:
%       copy_term(Var1, Var2)
%       Copia o conteúdo de Var1 em Var2, nas linguagens 
%       imperativas seria algo semelhante a:
%       Var2 = Var1
uselapis :-
    nb_getval(pencil, Pencil),
    check_xy(Id),
    copy_term(Id, New_id),
    (
        Pencil =:= 0 -> Final_id is New_id+1,
                        nb_setval(actual_id, Final_id),
                        write('Id atual: '), print(Final_id), nl,
                        new0(Final_id), !;
                nb_setval(pencil, 1), !
    ).

% Repete o comando especificado N vezes
%
% N = Número de vezes para repetir o comando
% Command = Comando desejado
repita(N, Command) :-
    consult('gramatica.pl'),
    between(1, N, _),
    cmd(Command),
    false.

% Cria os pontos necessários para uma estrela de 12 pontas de tamanho Size
%
% Melga Approves!
estrela(Size) :-
    between(1, 11, _),
    parafrente(Size),
    giradireita(150),
    false.

% Salva o conteúdo modificado no banco de dados,
% ou seja, em desenhos.pl
commit :-
    open('desenhos.pl', write, Stream),
    telling(Screen),
    tell(Stream),
    listing(xy),
    listing(angle),
    listing(xylast/2),
    tell(Screen),
    close(Stream).

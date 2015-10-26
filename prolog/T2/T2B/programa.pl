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
     
   - Colocar o nome e matricula de cada integrante do grupo
     nestes comentarios iniciais do programa
*/

:- initialization(new0(id1)).

% Coloca tartaruga no centro da tela (de 1000x1000)
% Implementacao incompleta:
%   - Considera apenas id1 e efetua new sem verificar sua existencia
%   - Supoe que ha' o xylast em 'desenhos.pl'
new0(Id) :-
    consult('gramatica.pl'),
    load,
    uselapis,
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

% Cria uma nova posição com o ângulo em relação a X.
% A medição considerada do ângulo é em graus.
new_angle(Id, Angle) :-
    retractall(angle(Id, _)),
    asserta(angle(Id, Angle)).

% Limpa os desenhos e reinicia no centro da tela (de 1000x1000)
% Implementacao incompleta:
%   - Considera apenas id1
tartaruga(Id) :-
    retractall(xy(_,_,_)),
    new(Id, 500, 500),
    retractall(xylast(_,_)),
    retractall(angle(_, _)),
    asserta(xylast(500, 500)).

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
    Destination_X is N*sin(Radian),
    Destination_Y is N*cos(Radian),
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
    Destination_X is (N*sin(Radian))*(-1),
    Destination_Y is (N*cos(Radian))*(-1),
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
    New_degree is Degree - G,
    write('GIRANDO À DIREITA'), nl,
    write('Posicionamento anterior (ângulo em graus): '), print(Degree), nl,
    write('Posicionamento atual (ângulo em graus): '), print(New_degree), nl,
    new_angle(_, New_degree).
    %write('Implementar: gd '), writeln(G).

% Gira a esquerda G graus
giraesquerda(G) :-
    angle(_, Degree),
    New_degree is Degree + G,
    write('GIRANDO À ESQUERDA'), nl,
    write('Posicionamento anterior (ângulo em graus): '), print(Degree), nl,
    write('Posicionamento atual (ângulo em graus): '), print(New_degree), nl,
    new_angle(_, New_degree).
    %write('Implementar: ge '), writeln(G).

% Use nada (levanta lapis)
usenada :-
    nb_setval(pencil, 0).
    %write('Implementar: un ').

% Use lapis
uselapis :-
    nb_setval(pencil, 1).
    %write('Implementar: un ').

%---------------------------------------------------

% Apaga os predicados 'xy' da memória e carrega os desenhos a partir de um arquivo de banco de dados
load :-
    retractall(xy(_,_,_)),
    open('desenhos.pl', read, Stream),
    repeat,
        read(Stream, Data),
        (Data == end_of_file -> true ; assert(Data), fail),
        !,
        close(Stream).

% Ponto de deslocamento, se <Id> existente
new(Id,X,Y) :-
    xy(Id,_,_),
    assertz(xy(Id,X,Y)),
    asserta(log(Id,X,Y)),
    !.

% Ponto inicial, caso contrário
new(Id,X,Y) :-
    asserta(xy(Id,X,Y)),
    asserta(log(Id,X,Y)),
    !.

% Exibe opções de busca
search :-
    write('searchAll(Id).     -> Ponto inicial e todos os deslocamentos de <Id>'), nl,
    write('searchFirst(Id,N). -> Ponto inicial e os <N-1> primeiros deslocamentos de <Id>'), nl,
    write('searchLast(Id,N).  -> Lista os <N> ultimos deslocamentos de <Id>').

searchAll(Id) :-
    listing(xy(Id,_,_)).

% Lista o ponto inicial e os N-1 primeiros pontos.
% Funções:
%       nth1(Index, List, Elem)
%       Supondo uma lista cuja primeira posição é em um.
%       Dado um número inteiro em Index representando a
%       posição de um elemento na lista List, este elemento
%       é armazenado em Elem.
%
%       between(Low, High, Value)
%       Low e High são inteiros, então Low <= High.
%       Se Value é um inteiro, então a sentença é
%       verdadeira se Low <= Value <= High. Nesta
%       regra, between é usado como um while loop,
%       que em C++ seria algo como: 
%           unsigned int Value = Low;
%           while (Value <= High) {
%               Value++;
%           }
searchFirst(Id, N) :-
    findall(
                Points,
                (
                    xy(Id, X, Y),
                    append([Id], [X], ListX),
                    append(ListX, [Y], Points)
                ), 
                All
            ),
    between(1, N, Middle), % Primeira iteração: Middle = 1.
    nth1(Middle, All, Element), % Pega o primeiro elemento de All.
    print(Element), nl, % Imprime o elemento coletado.
    false.  % Condição não satisfeita, volta para between
            % onde Middle passa a valer 2, o segundo elemento é
            % coletado e impresso e assim sucessivamente 
            % até Middle = N.

% Lista os N últimos deslocamentos de Id.
% Função:
%       nth0(Index, List, Elem)
%       Supondo uma lista cuja primeira posição é em zero.
%       Mesmo funcionamento de nth1().
searchLast(Id, N) :-
    findall(
                Points, 
                (
                    xy(Id, X, Y),
                    append([Id], [X], ListX),
                    append(ListX, [Y], Points)
                ),
                All
            ),
    length(All, Size),
    Start is Size-N,
    between(Start, Size, Middle),
    nth0(Middle, All, Element),
    print(Element), nl,
    false.

% Exibe opcões de alteração
change :-
    write('change(Id,X,Y,Xnew,Ynew).  -> Altera um ponto de <Id>'), nl,
    write('changeFirst(Id,Xnew,Ynew). -> Altera o ponto inicial de <Id>'), nl,
    write('changeLast(Id,Xnew,Ynew).  -> Altera o deslocamento final de <Id>').

% Altera o elemento da lista.
%   Operação:
%       condições (if)
%       Em SWI-Prolog o funcionamento de uma clásula de
%       condição é definido da seguinte forma:
%           (condição_if -> cláusula_then; cláusula_else)
%
change(Id, X, Y, Xnew, Ynew) :-
    (findall(
                Points, % busca por todos os pontos do banco de dados
                (
                    xy(Idgeneric, Xgeneric, Ygeneric), 
                    append([Idgeneric], [Xgeneric], ListX), 
                    append(ListX, [Ygeneric], Points)
                ), % coletando todos os pontos do banco de dados
                All % lista com todos os pontos
            ),
    length(All, Size),
    retractall(xy(_,_,_)),
    retractall(log(_,_,_)),
    between(0, Size, Middle),
    nth0(Middle, All, Element),
    nth0(0, Element, NewId),
    nth0(1, Element, ElemX),
    nth0(2, Element, ElemY),
    (Id = NewId, X = ElemX , Y = ElemY ->  new(Id, Xnew, Ynew);
                                           new(NewId, ElemX, ElemY)), false);
    true.

% Altera o primeiro elemento da lista.
changeFirst(Id, Xnew, Ynew) :-
    remove(Id, _, _), !,
    asserta(xy(Id, Xnew, Ynew)), !.

% Altera o último elemento da lista.
% Função:
%       last(List, Elem)
%       Armazena em Elem o último elemento da lista List.
changeLast(Id, Xnew, Ynew) :-
    findall(
                Points, 
                (
                    xy(Id, X, Y),
                    append([Id], [X], ListX),
                    append(ListX, [Y], Points)
                ), 
                All
            ),
    last(All, Elem),
    nth0(0, Elem, K), % Id
    nth0(1, Elem, J), % X
    nth0(2, Elem, L), % Y
    remove(K, J, L),
    assertz(xy(Id, Xnew, Ynew)).

% Remove um deslocamento ou ponto
remove(Id, X, Y) :-
    retract(xy(Id, X, Y)).

% Remove todos os pontos e deslocamentos de um Id
removeAll(Id) :-
    retractall(xy(Id, _, _)), !.

% Remove a adição mais recente feita na database.
undo :-
  log(E, P, K),
  retract(log(E, P, K)),
  remove(E, P, K), !.

% Grava os desenhos da memória em arquivo
commit :-
    open('desenhos.pl', write, Stream),
    telling(Screen),
    tell(Stream),
    listing(xy),
    tell(Screen),
    close(Stream).

% Cria um ponto inicial e os deslocamentos necessários para desenhar um quadrado
quadrado(Id, X, Y, Lado) :-
    Negative is Lado*(-1),
    new(Id, X, Y),
    new(Id, Lado, 0),
    new(Id, 0, Lado),
    new(Id, Negative, 0).

% Cria um ponto inicial e os deslocamentos necessários para desenhar um octógono
figura(Id, X, Y, Lado) :-
    K is ((Lado * sqrt(2)) / 2),
    Negative_K is K*(-1),
    Negative_Lado is Lado*(-1),
    new(Id, X, Y),
    new(Id, Lado, 0),
    new(Id, K, K),
    new(Id, 0, Lado),
    new(Id, Negative_K, K),
    new(Id, Negative_Lado, 0),
    new(Id, Negative_K, Negative_K),
    new(Id, 0, Negative_Lado).

% Replica a figura do Id N vezes com deslocamento Dx e Dy
replica(Id, N, Dx, Dy) :-
    between(1, N, K),
    replicaAux(Id, K, Dx, Dy),
    false.

% Replica a figura uma vez
% Função:
%       atom_concat(Atom1, Atom2, Atom3)
%       A concatenação de Atom1 e Atom2 é armazenada em Atom3.
replicaAux(Id, K, Dx, Dy) :-
    findall(
                Points,
                (
                    xy(Id, X, Y),
                    append([Id], [X], ListX),
                    append(ListX, [Y], Points)
                ), 
                All
            ),
    length(All, Size),
    between(0, Size, Middle),
    nth0(Middle, All, J),
    nth0(0, J, IdNew),
    nth0(1, J, XNew),
    nth0(2, J, YNew),
    atom_concat(IdNew, '_r', IdAux),
    atom_concat(IdAux, K, IdFinal),
    XFinal is XNew + (Dx * K),
    YFinal is YNew + (Dy * K),
    ((Middle =:= 0) -> new(IdFinal, XFinal, YFinal) ; 
                       new(IdFinal, XNew, YNew)),
    false.

% Exibe menu principal
menu :-
    write('load.        -> Carrega todos os desenhos do banco de dados para a memória'), nl,
    write('new(Id,X,Y). -> Insere um deslocamento no desenho com identificador <Id>'), nl,
    write('                (se primeira inserção, trata-se de um ponto inicial)'), nl,
    write('search.      -> Consulta pontos dos desenhos'), nl,
    write('change.      -> Modifica um deslocamento existente do desenho'), nl,
    write('remove.      -> Remove um determinado deslocamento existente do desenho'), nl,
    write('removeAll.   -> Remove todos os pontos e deslocamentos de um Id'), nl,
    write('quadrado.    -> Insere os pontos e deslocamentos para um quadrado.'), nl,
    write('figura.      -> Insere os pontos e deslocamentos para um octógono.'), nl,
    write('replica.     -> Replica a figura N vezes com deslocamento.'), nl,
    write('undo.        -> Remove o deslocamento inserido mais recentemente'), nl,
    write('commit.      -> Grava alterações de todos os desenhos no banco de dados').

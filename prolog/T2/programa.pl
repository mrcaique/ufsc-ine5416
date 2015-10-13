/*
   Programação Lógica - Prof. Alexandre G. Silva - 30set2015
   
    Adições sugeridas implementadas por:
            Caique Rodrigues Marques
            Gustavo José Carpeggiani
            Vinícius Couto Biermann

   RECOMENDAÇÕES:
   - O nome deste arquivo deve ser 'programa.pl'
   - O nome do banco de dados deve ser 'desenhos.pl'
   - Dicas de uso podem ser obtidas na execução: 
     ?- menu.
     
   - Exemplo de uso:
     ?- load.
     ?- searchAll(id1).
*/

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
%       verdadeira se Low <= Value <= High.
searchFirst(Id, N) :-
    findall(Points, (xy(Id, X, Y), append([Id], [X], ListX), append(ListX, [Y], Points)), All),
    between(1, N, Middle),
    nth1(Middle, All, Element),
    print(Element), nl,
    false.

% Lista os N últimos deslocamentos de Id.
% Função:
%       nth0(Index, List, Elem)
%       Supondo uma lista cuja primeira posição é em zero.
%       Mesmo funcionamento de nth1().
searchLast(Id, N) :-
    findall(Points, (xy(Id, X, Y), append([Id], [X], ListX), append(ListX, [Y], Points)), All),
    length(All, Size),
    Start is Size-N,
    between(Start, Size, Middle),
    nth0(Middle, All, Element),
    print(Element), nl,
    false.

% Exibe opcões de alteração
change :-
    write('change(Id,X,Y,Xnew,Ynew).  -> Altera o ponto inicial de <Id>'), nl,
    write('changeFirst(Id,Xnew,Ynew). -> Altera o ponto inicial de <Id>'), nl,
    write('changeLast(Id,Xnew,Ynew).  -> Altera o deslocamento final de <Id>').

% Altera o primeiro elemento da lista.
change(Id, X, Y, Xnew, Ynew) :-
    remove(Id, X, Y),
    asserta(xy(Id, Xnew, Ynew)), !.

% Altera o primeiro elemento da lista.
changeFirst(Id, Xnew, Ynew) :-
    remove(Id, _, _), !,
    asserta(xy(Id, Xnew, Ynew)), !.

% Altera o último elemento da lista.
% Função:
%       last(List, Elem)
%       Armazena em Elem o último elemento da lista List.
changeLast(Id, Xnew, Ynew) :-
    findall(Points, (xy(Id, X, Y), append([Id], [X], ListX), append(ListX, [Y], Points)), All),
    last(All, Elem),
    nth0(0, Elem, K), % Id
    nth0(1, Elem, J), % X
    nth0(2, Elem, L), % Y
    remove(K, J, L),
    assertz(xy(Id, Xnew, Ynew)).

% Remove um deslocamento ou ponto
remove(Id, X, Y) :-
    retract(xy(Id, X, Y)), !.

% Remove todos os pontos e deslocamentos de um Id
removeAll(Id) :-
    retractall(xy(Id, _, _)), !.

% Remove a adição mais recente feita na database.
undo:-
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
    new(Id, X, Y),
    new(Id, Lado, 0),
    new(Id, 0, Lado),
    new(Id, -Lado, 0).

% Cria um ponto inicial e os deslocamentos necessários para desenhar um octógono
figura(Id, X, Y, Lado) :-
    K is ((Lado * sqrt(2)) / 2),
    new(Id, X, Y),
    new(Id, Lado, 0),
    new(Id, K, K),
    new(Id, 0, Lado),
    new(Id, -K, K),
    new(Id, -Lado, 0),
    new(Id, -K, -K),
    new(Id, 0, -Lado).

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
    findall(Points, (xy(Id, X, Y), append([Id], [X], ListX), append(ListX, [Y], Points) ), All),
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

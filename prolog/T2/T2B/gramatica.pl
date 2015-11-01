/*
   Programação Lógica - Prof. Alexandre G. Silva - UFSC
     Ultima atualização: 15out2015
   
   Gramática para implementação de um subconjunto de comandos da Linguagem LOGO
   
   Área de teste:
     - https://turtleacademy.com/playground/pt

   Documentação:
     - pf número (ex: pf 50)
         'para frente' - avança o número de passos indicados pelo número.
     - pt número (ex: pt 100)
         'para tras' - recua o número de passos indicados pelo número.
     - gd número (ex: gd 90)
         'gira à direita' - gira a direita tantos graus quanto indicados.
     - ge número (ex: ge 45)
         'gira à esquerda' - gira a esquerda tantos graus quanto indicados.
     - repita número <comando> (ex: repita 8<pf 50 gd 45>)
         Repete comando/programa tantas vezes quanto indicadas pelo número.
     - un
         'use nada' - levanta o lápis, não deixando mais traço ao se deslocar.
     - ul
         'use lápis' - usa o lápis, desenhando/escrevendo ao se deslocar.
     - tartaruga
         Limpa os desenhos e reinicia no centro da tela (origem).
     - estrela número (ex: estrela 100)
         Cria os pontos para desenhar uma estrela de 12 pontas de tamanho Size.

   Observação:
     - Não tente executar um comando sem antes carregar 'programa.pl'
*/


%---------------------------------------------------


programa --> [].
programa --> comando, programa.


comando --> [pf], [N],  { atom_number(N, X), parafrente(X) }.
comando --> [pt], [N],  { atom_number(N, X), paratras(X) }.
comando --> [gd], [G],  { atom_number(G, X), giradireita(X) }.
comando --> [ge], [G],  { atom_number(G, X), giraesquerda(X) }.
comando --> [repita], [N], bloco_inicio, programa, bloco_fim,  { atom_number(N, _) }.
comando --> [un],  { usenada }.
comando --> [ul],  { uselapis }.
comando --> [tartaruga],  { tartaruga }.
comando --> [estrela], [Size], { estrela }.


%---------------------------------------------------


bloco_inicio --> ['['].
bloco_fim --> [']'].


%---------------------------------------------------


eos([], []).

replace(_, _) --> call(eos), !.
replace(Find, Replace), Replace -->
        Find,
        !,
        replace(Find, Replace).
replace(Find, Replace), [C] -->
        [C],
        replace(Find, Replace).

substitute(Find, Replace, Request, Result):-
        phrase(replace(Find, Replace), Request, Result).    

remove([],_,[]) :- !. 
remove([X|T],X,L1) :- !, remove(T,X,L1).         
remove([H|T],X,[H|L1]) :- remove(T,X,L1).


%---------------------------------------------------


cmd(Comando) :-
    substitute("[", " [ ", Comando, R1),
    atom_codes(_, R1),
    substitute("]", " ] ", R1, R2),
    atom_codes(A2, R2),
    atomic_list_concat(L1, ' ', A2),
    remove(L1, '', Cmd),
    programa(Cmd, []).


%---------------------------------------------------
% Referências:
% http://www.pathwayslms.com/swipltuts/dcg/
% https://en.wikibooks.org/wiki/Prolog/Definite_Clause_Grammars
% http://stackoverflow.com/questions/6392725/using-a-prolog-dcg-to-find-replace-code-review

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Créditos: 
%	Caique Rodrigues Marques,
%	Gustavo José Carpeggiani,
%	Vinícius Couto Biermann
%
% As regras a seguir correspondem à segunda implementação
% baseada na base de dados, onde se faz o uso de listas e
% de funções nativas do SWI-Prolog, cada uma está especifi-
% cada com um pouco quando é referenciada pela primeira vez.
%
% Mais detalhes das funções na documentação do SWI-Prolog,
% disponível em: http://www.swi-prolog.org/
%
% NOTAS:
% 1) Este arquivo contém a dependência da base de dados.
% Todas as regras fomuladas aqui são baseadas no arquivo
% "database.pl".
% 2) Veja o arquivo "curriculum_grid.pl" que contém as três
% partes do projeto (database.pl, t1b.pl e t1c.pl) na íntegra.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				DEPENDÊNCIAS				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 	* database.pl
% 	--
%	No terminal do swi-prolog, digite:
%		?-[database, t1b, t1c].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				REGRAS					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) Número de disciplinas em uma dada fase
%           No terminal do swi-prolog, basta digitar
%           "num_disciplinas_fase(F, N)", onde:
%
% F = Fase desejada.
% N = Número de disciplinas da fase F.
num_disciplinas_fase(F, N) :- fase(F, X), length(X, N).

% 2) Número de disciplinas no curso (excluindo optativas)
%	Funções:
%	1) bagof(Template, Meta do template, Lista instanciada)
%		Cria listas com instâncias das metas cumpridas, ao usar
%		o operador "^" (ou "**") a função mostra uma lista comple-
%		ta com todas as soluções das metas do template.
%	2) sum_list(L, S)
%		Soma S é o resultado da adição de todos os membros da lista L.
%	3) expressão1^expressão2
%		Realiza um operação de potência, neste caso, mostra
%		todas as os número de opções em que C possui pré-
%		requisito.
num_disciplinas_curso(N) :-
		bagof(Z, C^(num_disciplinas_fase(C, Z)), X),
		sum_list(X, N).

% 3) Número de disciplinas que têm pré-requisitos
%	Funções:
%	1) setof(template, meta do template, lista instanciada).
%		Tem quase o mesmo funcionamento que o "bagof", no
%		entanto, a lista instanciada é ordenada e sem repetições.
%	2) length(lista, tamanho)
%		Retorna o tamanho de uma lista
num_pos_req(A) :-
		setof(Z, Disciplinas^(depende(Z, Disciplinas)), ListReq),
		length(ListReq, A).

% 4) Número disciplinas que são pré-requisitos
%
% A = Número de disciplinas que são pré-requisitos
num_pre_req(A) :-
		setof(Z, Disciplinas^(depende(Disciplinas, Z)), ListPreReq),
		length(ListPreReq, A).

% 5) Número de pré-requisitos para uma dada disciplina D
%		Dada uma disciplina D, a função conta todas as disciplinas
%		em que D é pré-requisito, sem contar encadeamentos. Por
%		exemplo, dadas disciplinas D, E e F sendo que depende(D, E)
%		e depende(E, F):
%				D <---- E <---- F
%		A função contabiliza apenas E.
%
% D = Disciplina.
% L = Número de pré-requisitos de D.
num_disc_pre_req(D, L) :-
		setof(Z, depende(D, Z), ListPreReq),
		length(ListPreReq, L).

% 6) Disciplina com a maior quantidade de pré-requisitos
%	Funções:
%	1) findall(template, meta do template, lista instanciada)
%		Tem quase o mesmo funcionamento de "bagof", no entanto,
%		findall reúne todas as soluções possíveis. Se não houver
%		soluções, findall retorna uma lista vazia.
%	2) max_list(lista, elemento)
%		Retorna o maior elemento de uma lista (fica armazenado
%		em "elemento").
maior_pre_req(Disciplina) :-
		findall(Tamanho, num_disc_pre_req(_, Tamanho), ListNumPreReq),
		max_list(ListNumPreReq, N),
		num_disc_pre_req(Disciplina, N).

% 7) Número de disciplinas que têm como pré-requisito uma dada disciplina D
%
% D = Disciplina.
% L = Número de disciplinas que tem como pré-requisito D
pre_req(D, L) :-
		bagof(D, Disciplinas^(depende(Disciplinas, D)), PosReqList),
		length(PosReqList, L).

% 8) Disciplina que é pré-requisito da maior quantidade de disciplinas
%	(mais importante)
%		num_disc_pos_req(D, L)
%		Dado o código de uma disciplina D, a função retorna o número
%		de disciplinas em que D é pré-requisito (o número fica armazenado em L),
%		ou seja, quantas disicplinas D acaba trancando. O contrário também
%		pode ser aplicado, ou seja, aplicando a quantidade L primeiro.
% D = Disciplina
% L = Número de disciplinas que dependem de D
num_disc_pos_req(D, L) :-
		setof(Z, depende(Z, D), PosReqList),
		length(PosReqList, L).

mais_importante(Disciplina) :-
		findall(Tamanho, num_disc_pos_req(_, Tamanho), ListNumReq),
		max_list(ListNumReq, Max),
		num_disc_pos_req(Disciplina, Max).

% 9) O maior encadeamento de pré-requisitos
%		num_disc_pre_req_enc(D, L)
%		Dado o código de uma disciplina D, a função retorna o número de dis-
%		ciplinas em que D depende, incluindo encadeamentos. Por exemplo, dadas
%		as disciplinas D, E e F, onde depende(D, E), depende(E, F):
%				D <---- E <---- F
%		Se aplicar D em num_disc_pre_req_enc(), a função contabiliza E e F.
% D = Disciplina
% L = Número de disciplinas em que D depende
%
% maior_encadeamento(Max, Disciplina)
% Max = Número de disciplinas do encadeamento.
% Disciplina = Disciplina que possui o maior encadeamento.
num_disc_pre_req_enc(D, L) :-
		setof(Z, rec_depende(D, Z), PreReqSet),
		length(PreReqSet, L).

maior_encadeamento(Max, Disciplina) :-
		findall(Tamanho, num_disc_pre_req_enc(_, Tamanho), List),
		max_list(List, Max),
		num_disc_pre_req_enc(Disciplina, Max).

% 10) Dada uma lista de disciplinas, retornar a quantidade total e quais
% são os seus pré-requisitos
%		Para cada disciplina em uma dada lista, retornará uma lista com
%		suas dependências diretas e sua quantidade.
%
%	Funções:
%	1) nl - Adiciona uma nova linha.
%	2) write('String') - Imprime na tela o conteúdo em 'String'
%	3) print(Z) - Imprime o conteúdo de 'Z'.
pre_req_lista([]) :- !.
pre_req_lista([H|T]) :- 
		setof(Z, H^(depende(H, Z)), X), 
		length(X, N),
		nl,
		write('Disciplina: '),
		print(H),
		nl,
		write('Lista de dependências: '),
		print(X),
		nl,
		write('Número de dependências: '),
		print(N),
		nl,
		pre_req_lista(T).

% 11) Encontrar o menor encadeamento de pré-requisitos
% menor_encadeamento(Min, Disciplina)
%	Funções:
%		min_list(Lista, Elemento)
%		Retorna o menor elemento de uma lista (fica armazenado
%		em "elemento").
%
% Min = Número de disciplinas do encadeamento.
% Disciplina = Disciplina que possui o menor encadeamento.
menor_encadeamento(Min, Disciplina) :-
		findall(Tamanho, num_disc_pre_req_enc(_, Tamanho), List),
		min_list(List, Min),
		num_disc_pre_req_enc(Disciplina, Min).

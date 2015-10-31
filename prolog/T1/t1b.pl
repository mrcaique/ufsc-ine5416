%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Créditos:
%	Caique Rodrigues Marques,
%	Gustavo José Carpeggiani,
%	Vinícius Couto Biermann
%
% As seguintes regras correspondem à primeira imple-
% mentação baseada na base de dados e a motivação é
% começar a explorar a base de dados, formulando re-
% gras para que consultas sejam realizadas.
%
% As regras são definidas assim:
% 		Cabeça :- Corpo
% Correspondendo a uma fórmula baseada na lógica de 
% predicados: Se "p" então "q". A condição é satis-
% feita se o conteúdo em Corpo for verdade, então o
% conteúdo de Cabeça também é verdade. Inclusive,
% baseando-se nos fatos definidos na base de dados,
% é possível achar possíveis soluções para uma dada
% entrada definida em "Cabeça". 
%
% NOTA:
% 1) Este arquivo contém a dependência da base de dados.
% Todas as regras fomuladas aqui são baseadas no arquivo
% "database.pl".
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- compile('database.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				REGRAS					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) Determinar a fase de uma disciplina:
%		Para verificar a fase de uma disciplina,
%		basta usar a cláusula "disciplina", es-
%		pecificar o código, o nome e usar um
%		operador como fase e o terminal do swi-
%		prolog o mostrará.
%
%	Exemplo:
%	?- disciplina(ine5433, 'tcc I', F).
%	F = f7.

% 2) Determinar o nome de uma disciplina:
%		Para verificar o nome de uma disciplina,
%		basta usar a cláusula "disciplina", es-
%		pecificar o código, a fase e usar um
%		operador como nome e o terminal do swi-
%		prolog o mostrará.
%
%	Exemplo:
%	?- disciplina(ine5408, N, f3).
%	N = 'estutura de dados'.

% 3) Determinar as disciplinas de uma fase:
%		Para verificar as disciplinas de uma fase,
%		basta usar a cláusula "disciplina", es-
%		pecificar a fase e usar operadores no nome
%		e no codigo. O terminal do swi-prolog mostrará
%		uma opçao. Pressione ';' para ver as opçoes
%		restantes.
%
%	Exemplo:
%	?- disciplina(C, N, f2).
%	C = ine5404,
%	N = 'programação orientada a objetos II' ;
%	C = ine5405,
%	N = 'probabilidade e estatística' ;
%	C = ine5406,
%	N = 'sistemas digitais' ;
%	C = ine5407,
%	N = 'ciência, tecnologia e sociedade' ;
%	C = mtm7174,
%	N = 'cálculo B para computação' ;
%	C = mtm5512,
%	N = 'geometria analítica'.

% 4) Disciplinas que têm pré-requisito (imediatamente
% anterior) em comum
%
% Z = Disciplina em comum;
% X, Y = Disciplinas.
requisito_anterior_em_comum(Z, X, Y) :- depende(X, Z), depende(Y, Z), not(X=Y).

% 5) Disciplinas que são pré-requisitos de pré-
% requisitos (árvore de dependências)
%
% X, Y, Z = Disciplinas.
rec_depende(X, Z) :- depende(X, Z).
rec_depende(X, Z) :- depende(X, Y), rec_depende(Y, Z).

% 6) Disciplinas que estão em uma determinada fase
% e são pré-requisitos de outras disciplinas.
%		No terminal do swi-prolog, basta digitar
%		"e_requisito(N, F)", onde:
%
% N = Nome da(s) disciplina(s) da fase que é um pré-requisito;
% F = Fase da(s) respectiva(s) disciplina(s).
e_requisito(N, F) :- depende(_, Y), disciplina(Y, N, F).

% 7) Disciplinas que estão em uma determinada fase
% e têm pré-requisitos para serem cursadas.
%		No terminal do swi-prolog, basta digitar
%		"tem_pre_requisito(N, F)", onde:
%
% N = Nome da(s) disciplina(s) da fase que possuem pré-requisitos;
% F = Fase da(s) respectiva(s) disciplina(s).
tem_pre_requisito(N, F) :- depende(Y, _), disciplina(Y, N, F).

% 8) Disciplinas que estão em uma determinada fase,
% têm pré-requisitos em comum e são pré-requisitos
% de outras disciplinas.
%		Supondo um grafo de quatro vétices X,
%		Y, Z e A e as areastas não direcionadas
%		ligando-os da seguinte forma: (X,Y),
%		(X,Z), (Y,A) e (Z, A), onde:
%
% X, Y, Z e A = Disciplinas.
requisito_em_comum(X, Y, Z, A) :- depende(X, Y),
			depende(X, Z),
			depende(Z, A),
			depende(Y, A).

% 9) Lista de dependências de uma disciplina
%		No terminal do swi-prolog, basta digitar
%		"lista_deps(X, N, F)", onde:
%
% X = código da disciplina;
% N = Nome da(s) disciplina(s) dependente(s);
% F = Fase da(s) respectiva(s) disciplina(s) dependente(s).
lista_deps(X, N, F) :- rec_depende(X, Y), disciplina(Y, N, F).

% 10) Lista de disciplinas subsequentes
%		No terminal do swi-prolog, basta digitar
%		"lista_sups(X, N, F)", onde:
%
% X = código da disciplina;
% N = Nome da(s) disciplina(s) subsequente(s);
% F = Fase da(s) respectiva(s) disciplina(s) subsequente(s).
lista_sups(X, N, F) :- depende(Y, X), disciplina(Y, N, F).

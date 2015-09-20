%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2015 Caique Rodrigues Marques,
%			Gustavo José Carpeggiani,
%			Vinícius Couto Biermann
%
% A seguinte base de dados representa a grade curricular
% do curso de bacharelado em ciências da computação da
% Universidade Federal de Santa Catarina (ufsc.br).
%
% As cláusulas são definidas em disciplinas e dependên-
% cias, onde cada uma é composta da seguinte forma:
%	- fase(f[número da fase]);
%	- disciplina(código, nome, fase);
%	- depende(disciplina escolhida, dependência).
%
% NOTAS:
% 1) A grade curricular se baseia no currículo
% de 2007/1;
% 2) As disciplinas optativas são tais que fase = 0;
% 3) Há disciplinas optativas que podem não estar presen-
% tes em um semestre.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				FATOS					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*Listas de fases e suas disciplinas*/
fase(f0, []).
fase(f1,[ine5401, ine5402, ine5403, mtm5161, eel5405]).
fase(f2,[ine5404, ine5405, ine5406, ine5407, mtm7174, mtm5512]).
fase(f3,[ine5408, ine5409, ine5410, ine5411, mtm5245]).
fase(f4,[ine5412, ine5413, ine5414, ine5415, ine5416, ine5417]).
fase(f5,[ine5418, ine5419, ine5420, ine5421, ine5422, ine5423]).
fase(f6,[ine5424, ine5425, ine5426, ine5427, ine5430, ine5453]).
fase(f7,[ine5428, ine5429, ine5431, ine5432, ine5433]).
fase(f8,[ine5434]).

/* Lista de disciplinas */
/* Primeira fase */
disciplina(ine5401, 'introdução à computação', f1).
disciplina(ine5402, 'programação orientada a objetos I', f1).
disciplina(ine5403, 'fundamentos da matemática discreta para a computação', f1).
disciplina(mtm5161, 'cálculo A', f1).
disciplina(eel5405, 'circuitos e técnicas digitais', f1).

/* Segunda fase */
disciplina(ine5404, 'programação orientada a objetos II', f2).
disciplina(ine5405, 'probabilidade e estatística', f2).
disciplina(ine5406, 'sistemas digitais', f2).
disciplina(ine5407, 'ciência, tecnologia e sociedade', f2).
disciplina(mtm7174, 'cálculo B para computação', f2).
disciplina(mtm5512, 'geometria analítica', f2).

/* Terceira fase */
disciplina(ine5408, 'estutura de dados', f3).
disciplina(ine5409, 'cálculo numérico para computação', f3).
disciplina(ine5410, 'programação concorrente', f3).
disciplina(ine5411, 'arquitetura e organização de computadores', f3).
disciplina(mtm5245, 'álgebra linear', f3).

/* Quarta fase */
disciplina(ine5412, 'sistemas operacionais I', f4).
disciplina(ine5413, 'grafos', f4).
disciplina(ine5414, 'redes de computadores I', f4).
disciplina(ine5415, 'teoria da computação', f4).
disciplina(ine5416, 'paradigmas da programação', f4).
disciplina(ine5417, 'engenharia de software I', f4).

/* Quinta fase */
disciplina(ine5418, 'computaçao distribuída', f5).
disciplina(ine5419, 'engenharia de software II', f5).
disciplina(ine5420, 'computação gráfica', f5).
disciplina(ine5421, 'linguagens formais e compiladores', f5).
disciplina(ine5422, 'redes de computadores II', f5).
disciplina(ine5423, 'banco de dados I', f5).

/* Sexta fase */
disciplina(ine5424, 'sistemas operacionais II', f6).
disciplina(ine5425, 'modelagem e simulaçao', f6).
disciplina(ine5426, 'construção de compiladores', f6).
disciplina(ine5427, 'planejamento e gestão de projetos', f6).
disciplina(ine5430, 'inteligência artificial', f6).
disciplina(ine5453, 'introdução ao tcc', f6).

/* Sétima fase */
disciplina(ine5428, 'informática e sociedade', f7).
disciplina(ine5429, 'segurança em computação', f7).
disciplina(ine5431, 'sistemas multimídia', f7).
disciplina(ine5432, 'banco de dados II', f7).
disciplina(ine5433, 'tcc I', f7).

/* Oitava fase */
disciplina(ine5434, 'tcc II', f8).

/* Optativas */
disciplina(ine5461, 'programa de intercâmbio I', f0).
disciplina(ine5462, 'programa de intercâmbio II', f0).
disciplina(ine5463, 'programa de intercâmbio III', f0).
disciplina(cad5146, 'marketing pessoal em informática', f0).
disciplina(cad5240, 'aspectos comportamentais do empreendedor', f0).
disciplina(cad5241, 'recursos humanos em informática', f0).
disciplina(ine5435, 'integração software/hardware', f0).
disciplina(ine5436, 'arquitetura de computadores I', f0).
disciplina(ine5437, 'arquitetura de computadores II', f0).
disciplina(ine5438, 'laboratório de microprocessadores e lógica programável', f0).
disciplina(ine5439, 'sistemas embarcados', f0).
disciplina(ine5440, 'tópicos especiais em arquitetura de computadores', f0).
disciplina(ine5441, 'sistemas de tempo real', f0).
disciplina(ine5442, 'circuitos e sistemas integrados', f0).
disciplina(ine5443, 'reconhecimento de padrões', f0).
disciplina(ine5444, 'estágio supervisionado I', f0).
disciplina(ine5445, 'estágio supervisionado II', f0).
disciplina(ine5446, 'tópicos especiais em sistemas de infraestrutura I', f0).
disciplina(ine5447, 'tópicos especiais em sistemas de infraestrutura II', f0).
disciplina(ine5448, 'tópicos especiais em aplicaçoes tecnológicas I', f0).
disciplina(ine5449, 'tópicos especiais em aplicaçoes tecnológicas II', f0).
disciplina(ine5450, 'tópicos especiais em aplicaçoes tecnológicas III', f0).
disciplina(ine5451, 'tópicos especiais em algoritmos I', f0).
disciplina(ine5452, 'tópicos especiais em algoritmos II', f0).
disciplina(ine5454, 'tópicos especiais em gerencia de dados', f0).
disciplina(ine5619, 'administraçao e gerência de redes de computadores', f0).
disciplina(ine5624, 'engenharia de usabilidade', f0).
disciplina(ine5628, 'sistemas multiagentes', f0).
disciplina(ine5640, 'computação móvel', f0).
disciplina(ine5643, 'data warehouse', f0).
disciplina(ine5644, 'data mining', f0).
disciplina(ine5646, 'programação para web', f0).
disciplina(ine5653, 'introdução à internacionalização e localização de software', f0).
disciplina(ine5656, 'introdução à lógica simbólica I', f0).
disciplina(ine5458, 'introdução à lógica simbólica II', f0).

/* Lista de dependências */
depende(ine5404, ine5402). % Dependências da segunda fase
depende(ine5405, mtm5161).
depende(ine5406, eel5405).
depende(mtm7174, mtm5161).

depende(ine5408, ine5404). % Dependências da terceira fase
depende(ine5409, mtm7174).
depende(ine5409, mtm5512).
depende(ine5410, ine5404).
depende(ine5411, ine5406).
depende(mtm5245, mtm5512).

depende(ine5412, ine5410). % Dependências da quarta fase
depende(ine5412, ine5411).
depende(ine5413, ine5408).
depende(ine5413, ine5403).
depende(ine5414, ine5404).
depende(ine5415, ine5408).
depende(ine5415, ine5403).
depende(ine5416, ine5408).
depende(ine5417, ine5408).

depende(ine5418, ine5412). % Dependências da quinta fase
depende(ine5418, ine5414).
depende(ine5419, ine5417).
depende(ine5420, mtm7174).
depende(ine5420, mtm5245).
depende(ine5420, ine5408).
depende(ine5421, ine5415).
depende(ine5422, ine5414).
depende(ine5423, ine5408).

depende(ine5424, ine5412). % Dependências da sexta fase
depende(ine5425, ine5405).
depende(ine5426, ine5421).
depende(ine5427, ine5419).
depende(ine5430, ine5416).
depende(ine5430, ine5405).
depende(ine5430, ine5413).
depende(ine5453, ine5417).

depende(ine5428, ine5407). % Dependências da sétima fase
depende(ine5429, ine5403).
depende(ine5429, ine5415).
depende(ine5431, ine5414).
depende(ine5432, ine5423).
depende(ine5433, ine5427).
depende(ine5433, ine5453).

depende(ine5434, ine5433). % Dependência da oitava fase

depende(ine5462, ine5461). % Dependências das optativas
depende(ine5463, ine5462).
depende(ine5435, ine5411).
depende(ine5436, ine5411).
depende(ine5437, ine5411).
depende(ine5438, ine5411).
depende(ine5439, ine5411).
depende(ine5440, ine5411).
depende(ine5441, ine5412).
depende(ine5442, ine5411).
depende(ine5443, ine5420).
depende(ine5444, ine5417).
depende(ine5445, ine5417).
depende(ine5446, ine5412).
depende(ine5447, ine5412).
depende(ine5448, ine5412).
depende(ine5449, ine5412).
depende(ine5450, ine5412).
depende(ine5451, ine5403).
depende(ine5451, ine5408).
depende(ine5452, ine5403).
depende(ine5452, ine5408).
depende(ine5454, ine5423).
depende(ine5454, ine5432).
depende(ine5454, ine5616).
depende(ine5619, ine5422).
depende(ine5624, ine5419).
depende(ine5628, ine5430).
depende(ine5640, ine5422).
depende(ine5643, ine5417).
depende(ine5644, ine5423).
depende(ine5646, ine5417).
depende(ine5653, ine5419).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				REGRAS					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) Determinar a fase de uma disciplina:
%		Para verificar a fase de uma disciplina,
%		basta usar a cláusula "disciplina", es-
%		pecificar o código, o nome e usar um
%		operador como fase e o terminal do pro-
%		log o mostrará.
%
%	Exemplo:
%	?- disciplina(ine5433, 'tcc I', F).
%	F = f7.

% 2) Determinar o nome de uma disciplina:
%		Para verificar o nome de uma disciplina,
%		basta usar a cláusula "disciplina", es-
%		pecificar o código, a fase e usar um
%		operador como nome e o terminal do pro-
%		log o mostrará.
%
%	Exemplo:
%	?- disciplina(ine5408, N, f3).
%	N = 'estutura de dados'.

% 3) Determinar as disciplinas de uma fase:
%		Para verificar as disciplinas de uma fase,
%		basta usar a cláusula "disciplina", es-
%		pecificar a fase e usar operadores no nome
%		e no codigo. O terminal do prolog mostrará
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
requisito_anterior_em_comum(Z, X, Y) :- depende(X, Z), depende(Y, Z).

% 5) Disciplinas que são pré-requisitos de pré-
% requisitos (árvore de dependências)
%
% X, Y, Z = Disciplinas.
rec_depende(X, Z) :- depende(X, Z).
rec_depende(X, Z) :- depende(X, Y), rec_depende(Y, Z).

% 6) Disciplinas que estão em uma determinada fase
% e são pré-requisitos de outras disciplinas.
%		No terminal do prolog, basta digitar
%		"e_requisito(N, F)", onde:
%
% N = Nome da(s) disciplina(s) da fase que é um pré-requisito;
% F = Fase da(s) respectiva(s) disciplina(s).
e_requisito(N, F) :- depende(_, Y), disciplina(Y, N, F).

% 7) Disciplinas que estão em uma determinada fase
% e têm pré-requisitos para serem cursadas.
%		No terminal do prolog, basta digitar
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
%		No terminal do prolog, basta digitar
%		"lista_deps(X, N, F)", onde:
%
% X = código da disciplina;
% N = Nome da(s) disciplina(s) dependentes;
% F = Fase da(s) respectiva(s) disciplina(s) dependente(s).
lista_deps(X, N, F) :- depende(X, Y), disciplina(Y, N, F).

% 10) Lista de disciplinas subsequentes
%		No terminal do prolog, basta digitar
%		"lista_sups(X, N, F)", onde:
%
% X = código da disciplina;
% N = Nome da(s) disciplina(s) subsequente(s);
% F = Fase da(s) respectiva(s) disciplina(s) subsequente(s).
lista_sups(X, N, F) :- depende(Y, X), disciplina(Y, N, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% 1)Quantas disciplinas há em uma dada fase
%           No terminal do prolog, basta digitar
%           "num_disciplinas_fase(F, N)", onde:
%
% F = Fase desejada.
% N = Número de disciplinas da fase F.
num_disciplinas_fase(F, N):- fase(F, X), length(X, N).

% 2)Quantas disciplinas há no curso
num_disciplinas_curso(N) :- bagof(Z, C^(num_disciplinas_fase(C, Z)), X), sum_list(X, N).

% 3)Quantas disciplinas têm pré-requisitos
num_pos_req(A) :- setof(Z, Disciplinas^(depende(Z, Disciplinas)), SetPosReq), length(SetPosReq, A).

% 4)Quantas disciplinas são pré-requisitos
num_pre_req(A) :- setof(Z, Disciplinas^(depende(Disciplinas, Z)), SetPreReq), length(SetPreReq, A).

% 5)Quantos pré-requisitos há para uma dada disciplina
num_disc_pre_req(Z, L) :- bagof(Z, Disciplinas^(depende(Z, Disciplinas)), SetPreReq), length(SetPreReq, L).

% 6)Qual a disciplina com a maior quantidade de pré-requisitos

% 7)Quantas disciplinas têm como pré-requisito uma dada disciplina
pre_req(Z, L) :- bagof(Z, Disciplinas^(depende(Disciplinas, Z)), SetPosReq), length(SetPosReq, L).

% 8)Qual a disciplina é pré-requisito da maior quantidade de disciplinas (mais importante)

% 9)Dado o encadeamento de pré-requisitos (implementado, por exemplo, pelo 'seq' abaixo), qual o maior encadeamento identificado na matriz

% 10)Crie e implemente uma questão que envolva quantidade ou máximo/mínimo

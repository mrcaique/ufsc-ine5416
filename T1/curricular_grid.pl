%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2015 Caique Rodrigues Marques
%
% A seguinte base de dados representa a grade curricular 
% do curso de bacharelado em ciências da computação da
% Universidade Federal de Santa Catarina (ufsc.br).
%
% As cláusulas são definidas em disciplinas e dependên-
% cias, onde cada uma é composta da seguinte forma:
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
%				FATOS 					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Lista de disciplinas */
/* Primeira fase */
disciplina(ine5401, 'introdução à computaçao', 1).
disciplina(ine5402, 'programação orientada a objetos I', 1).
disciplina(ine5403, 'fundamentos da matemática discreta para a computação', 1).
disciplina(mtm5161, 'cálculo A', 1).
disciplina(eel5405, 'circuitos e técnicas digitais', 1).

/* Segunda fase */
disciplina(ine5404, 'programação orientada a objetos II', 2).
disciplina(ine5405, 'probabilidade e estatística', 2).
disciplina(ine5406, 'sistemas digitais', 2).
disciplina(ine5407, 'ciência, tecnologia e sociedade', 2).
disciplina(mtm7174, 'cálculo B para computação', 2).
disciplina(mtm5512, 'geometria analítica', 2).

/* Terceira fase */
disciplina(ine5408, 'estutura de dados', 3).
disciplina(ine5409, 'cálculo numérico para computação', 3).
disciplina(ine5410, 'programação concorrente', 3).
disciplina(ine5411, 'arquitetura e organização de computadores', 3).
disciplina(mtm5245, 'álgebra linear', 3).

/* Quarta fase */
disciplina(ine5412, 'sistemas operacionais I', 4).
disciplina(ine5413, 'grafos', 4).
disciplina(ine5414, 'redes de computadores I', 4).
disciplina(ine5415, 'teoria da computação', 4).
disciplina(ine5416, 'paradigmas da programação', 4).
disciplina(ine5417, 'engenharia de software I', 4).

/* Quinta fase */
disciplina(ine5418, 'computaçao distribuída', 5).
disciplina(ine5419, 'engenharia de software II', 5).
disciplina(ine5420, 'computação gráfica', 5).
disciplina(ine5421, 'linguagens formais e compiladores', 5).
disciplina(ine5422, 'redes de computadores II', 5).
disciplina(ine5423, 'banco de dados I', 5).

/* Sexta fase */
disciplina(ine5424, 'sistemas operacionais II', 6).
disciplina(ine5425, 'modelagem e simulaçao', 6).
disciplina(ine5426, 'construção de compiladores', 6).
disciplina(ine5427, 'planejamento e gestão de projetos', 6).
disciplina(ine5430, 'inteligência artificial', 6).
disciplina(ine5453, 'introdução ao tcc', 6).

/* Sétima fase */
disciplina(ine5428, 'informática e sociedade', 7).
disciplina(ine5429, 'segurança em computação', 7).
disciplina(ine5431, 'sistemas multimídia', 7).
disciplina(ine5432, 'banco de dados II', 7).
disciplina(ine5433, 'tcc I', 7).

/* Oitava fase */
disciplina(ine5434, 'tcc II', 8).

/* Optativas */
disciplina(ine5461, 'programa de intercâmbio I', 0).
disciplina(ine5462, 'programa de intercâmbio II', 0).
disciplina(ine5463, 'programa de intercâmbio III', 0).
disciplina(cad5146, 'marketing pessoal em informática', 0).
disciplina(cad5240, 'aspectos comportamentais do empreendedor', 0).
disciplina(cad5241, 'recursos humanos em informática', 0).
disciplina(ine5435, 'integração software/hardware', 0).
disciplina(ine5436, 'arquitetura de computadores I', 0).
disciplina(ine5437, 'arquitetura de computadores II', 0).
disciplina(ine5438, 'laboratório de microprocessadores e lógica programável', 0).
disciplina(ine5439, 'sistemas embarcados', 0).
disciplina(ine5440, 'tópicos especiais em arquitetura de computadores', 0).
disciplina(ine5441, 'sistemas de tempo real', 0).
disciplina(ine5442, 'circuitos e sistemas integrados', 0).
disciplina(ine5443, 'reconhecimento de padrões', 0).
disciplina(ine5444, 'estágio supervisionado I', 0).
disciplina(ine5445, 'estágio supervisionado II', 0).
disciplina(ine5446, 'tópicos especiais em sistemas de infraestrutura I', 0).
disciplina(ine5447, 'tópicos especiais em sistemas de infraestrutura II', 0).
disciplina(ine5448, 'tópicos especiais em aplicaçoes tecnológicas I', 0).
disciplina(ine5449, 'tópicos especiais em aplicaçoes tecnológicas II', 0).
disciplina(ine5450, 'tópicos especiais em aplicaçoes tecnológicas III', 0).
disciplina(ine5451, 'tópicos especiais em algoritmos I', 0).
disciplina(ine5452, 'tópicos especiais em algoritmos II', 0).
disciplina(ine5454, 'tópicos especiais em gerencia de dados', 0).
disciplina(ine5619, 'administraçao e gerência de redes de computadores', 0).
disciplina(ine5624, 'engenharia de usabilidade', 0).
disciplina(ine5628, 'sistemas multiagentes', 0).
disciplina(ine5640, 'computação móvel', 0).
disciplina(ine5643, 'data warehouse', 0).
disciplina(ine5644, 'data mining', 0).
disciplina(ine5646, 'programação para web', 0).
disciplina(ine5653, 'introdução à internacionalização e localização de software', 0).
disciplina(ine5656, 'introdução à lógica simbólica I', 0).
disciplina(ine5458, 'introdução à lógica simbólica II', 0).

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
depende(ine5430, ine5408).
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
%				REGRAS 					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Árvore de dependências */
depende(X, Z) :- depende(X, Y), depende(Y, Z).

% Lista de dependências de uma disciplina
%
% X = código da disciplina;
% N = Nome da(s) disciplina(s) dependentes;
% F = Fase da(s) respectiva(s) disciplina(s) dependente(s).
lista_deps(X, N, F) :- depende(X, Y), disciplina(Y, N, F).

% Lista de disciplinas subsequentes
%
% X = código da disciplina;
% N = Nome da(s) disciplina(s) subsequente(s);
% F = Fase da(s) respectiva(s) disciplina(s) subsequente(s).
lista_sups(X, N, F) :- depende(Y, X), disciplina(Y, N, F).

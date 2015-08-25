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
%	- requisito(disciplina escolhida, dependência).
%
% NOTAS: 
% 1) A grade curricular se baseia no currículo
% de 2007/1;
% 2) As disciplinas optativas é tal que fase = 0.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								FATOS 									%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

requisito(ine5404, ine5402). % Para cursar ine5404, é necessario ter completado ine5402
requisito(ine5405, mtm5161). % ou seja, ine5402 é requisito de ine5404
requisito(ine5406, eel5405).
requisito(mtm7174, mtm5161).

/* Terceira fase */
disciplina(ine5408, 'estutura de dados', 3).
disciplina(ine5409, 'cálculo numérico', 3).
disciplina(ine5410, 'programação concorrente', 3).
disciplina(ine5411, 'arquitetura e organização de computadores', 3).
disciplina(mtm5245, 'álgebra linear', 3).

requisito(ine5408, ine5404).
requisito(ine5409, mtm7174).
requisito(ine5409, mtm5512).
requisito(ine5410, ine5404).
requisito(ine5411, ine5406).
requisito(mtm5245, mtm5512).

/* Quarta fase */
disciplina(ine5412, 'sistemas operacionais I', 4).
disciplina(ine5413, 'grafos', 4).
disciplina(ine5414, 'redes de computadores I', 4).
disciplina(ine5415, 'teoria da computação', 4).
disciplina(ine5416, 'paradigmas da programação', 4).
disciplina(ine5417, 'engenharia de software I', 4).

requisito(ine5412, ine5410).
requisito(ine5412, ine5411).
requisito(ine5413, ine5408).
requisito(ine5414, ine5404).
requisito(ine5415, ine5408).
requisito(ine5416, ine5408).
requisito(ine5417, ine5408).

/* Quinta fase */
disciplina(ine5418, 'computaçao distribuída', 5).
disciplina(ine5419, 'engenharia de software II', 5).
disciplina(ine5420, 'computação gráfica', 5).
disciplina(ine5421, 'linguagens formais e compiladores', 5).
disciplina(ine5422, 'redes de computadores II', 5).
disciplina(ine5423, 'banco de dados I', 5).

requisito(ine5418, ine5412).
requisito(ine5418, ine5414).
requisito(ine5419, ine5417).
requisito(ine5420, mtm7174).
requisito(ine5421, ine5415).
requisito(ine5422, ine5414).
requisito(ine5423, ine5408).

/* Sexta fase */
disciplina(ine5424, 'sistemas operacionais II', 6).
disciplina(ine5425, 'modelagem e simulaçao', 6).
disciplina(ine5426, 'construção de compiladores', 6).
disciplina(ine5427, 'planejamento e gestão de projetos', 6).
disciplina(ine5430, 'inteligência artificial', 6).
disciplina(ine5453, 'introdução ao tcc', 6).

requisito(ine5424, ine5412).
requisito(ine5425, ine5405).
requisito(ine5426, ine5421).
requisito(ine5427, ine5419).
requisito(ine5430, ine5416).
requisito(ine5430, ine5408).
requisito(ine5453, ine5419).

/* Setima fase */
disciplina(ine5428, 'informática e sociedade', 7).
disciplina(ine5429, 'segurança em computação', 7).
disciplina(ine5431, 'sistemas multimídia', 7).
disciplina(ine5432, 'banco de dados II', 7).
disciplina(ine5433, 'tcc I', 7).

requisito(ine5428, ine5407).
requisito(ine5429, ine5403).
requisito(ine5429, ine5415).
requisito(ine5431, ine5414).
requisito(ine5432, ine5423).	
requisito(ine5433, ine5427).
requisito(ine5433, ine5453).

/* Oitava fase */
disciplina(ine5434, 'tcc II', 8).

requisito(ine5434, ine5433).

/* Optativas */
disciplina(ine5461, 'programa de intercambio I', 0).
disciplina(ine5462, 'programa de intercambio II', 0).
disciplina(ine5463, 'programa de intercambio III', 0).
disciplina(cad5146, 'marketing pessoal em informática', 0).
disciplina(cad5240, 'aspectos comportamentais do empreendedor', 0).
disciplina(cad5241, 'recursos humanos em informática', 0).
disciplina(ine5435, 'integraçao software/hardware', 0).
disciplina(ine5436, 'arquitetura de computadores I', 0).
disciplina(ine5437, 'arquitetura de computadores II', 0).
disciplina(ine5438, 'laboratorio de microprocessadores e logica programavel', 0).
disciplina(ine5439, 'sistemas embarcados', 0).
disciplina(ine5440, 'topicos especiais em arquitetura de computadores', 0).
disciplina(ine5441, 'sistemas de tempo real', 0).
disciplina(ine5442, 'circuitos e sistemas integrados', 0).
disciplina(ine5443, 'reconhecimento de padroes', 0).
disciplina(ine5444, 'estagio supervisionado I', 0).
disciplina(ine5445, 'estagio supervisionado II', 0).
disciplina(ine5446, 'topicos especiais em sistemas de infraestrutura I', 0).
disciplina(ine5447, 'topicos especiais em sistemas de infraestrutura', 0).
disciplina(ine5448, 'topicos especiais em aplicaçoes tecnologicas I', 0).
disciplina(ine5449, 'topicos especiais em aplicaçoes tecnologicas II', 0).
disciplina(ine5450, 'topicos especiais em aplicaçoes tecnologicas III', 0).
disciplina(ine5451, 'topicos especiais em algoritmos I', 0).
disciplina(ine5452, 'topicos especiais em algoritmos II', 0).
disciplina(ine5454, 'topicos especiais em gerencia de dados', 0).
disciplina(ine5619, 'administraçao e gerencia de redes de computadores', 0).
disciplina(ine5624, 'engenharia de usabilidade', 0).
disciplina(ine5628, 'sistemas multiagentes', 0).
disciplina(ine5640, 'computação movel', 0).
disciplina(ine5643, 'data warehouse', 0).
disciplina(ine5644, 'data mining', 0).
disciplina(ine5646, 'programação para web', 0).
disciplina(ine5653, 'introdução a internacionalizaçao e localizaçao de software', 0).
disciplina(ine5656, 'introdução a logica simbolica I', 0).
disciplina(ine5458, 'introdução a logica simbolica II', 0).

requisito(ine5462, ine5461).
requisito(ine5463, ine5462).
requisito(ine5435, ine5411).
requisito(ine5436, ine5411).
requisito(ine5437, ine5411).
requisito(ine5438, ine5411).
requisito(ine5439, ine5411).
requisito(ine5440, ine5411).
requisito(ine5441, ine5412).
requisito(ine5442, ine5411).
requisito(ine5443, ine5420).
requisito(ine5444, ine5417).
requisito(ine5445, ine5417).
requisito(ine5446, ine5412).
requisito(ine5447, ine5412).
requisito(ine5448, ine5412).
requisito(ine5449, ine5412).
requisito(ine5450, ine5412).
requisito(ine5451, ine5403).
requisito(ine5451, ine5408).
requisito(ine5452, ine5403).
requisito(ine5452, ine5408).
requisito(ine5454, ine5423).
requisito(ine5454, ine5432).
requisito(ine5454, ine5616).
requisito(ine5619, ine5422).
requisito(ine5624, ine5419).
requisito(ine5628, ine5430).
requisito(ine5640, ine5422).
requisito(ine5643, ine5417).
requisito(ine5644, ine5423).
requisito(ine5646, ine5417).
requisito(ine5653, ine5419).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								REGRAS 									%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Árvore de dependências */
requisito(X, Z) :- requisito(X, Y), requisito(Y, Z).

/* É necessário ter cursado uma de três disciplinas para cursar ine5454 */
requisito_ine5454(A) :- requisito(ine5454, A).

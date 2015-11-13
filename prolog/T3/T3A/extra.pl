% Image processing package in Prolog (a initial tentative)
% Prof. A. G. Silva - UFSC - October 2015
%
% Extra functions, especially the transformation of coordinates list for matrix notation
% Example: 
%    ?- coord2matrix([(0,0,50),(0,1,10),(0,2,30),(1,0,10),(1,1,20),(1,2,40)], M).
%    M = [[50, 10, 30], [10, 20, 40]].

:- consult('imagem.pl').
:- consult('pgm.pl').

coord2matrix(S, M) :-
    height(S, H),
    matrixconstruct(S, H, -1, [], M),
    !.

test :-
    load('imgs/ufsc.pgm', S),
    coord2matrix(S, M),
    writePGM('ufsc_out.pgm', M),
    !.

load(FileName, S) :-
    readPGM(FileName, M),
    coord(M, S).

matrixconstruct(_, H, H, [_|Mt], M) :-
    reverse(Mt, M).
matrixconstruct(S, H, L, Macc, M) :-
    L1 is L + 1,
    findall( V, value(S,(L1,_,V)), Line ),
    matrixconstruct(S, H, L1, [Line|Macc], M).

shape(S, H, W) :-
    height(S, H), width(S, W).

height(S, H) :-
    findall( L, value(S,(L,0,_)), Ll ),
    max_list(Ll, H1),
    H is H1 + 1.

width(S, W) :-
    findall( C, value(S,(0,C,_)), Lc ),
    max_list(Lc, W1),
    W is W1 + 1.

value([(X,Y,V)|_], (X,Y,V)).
value([_|St], (X,Y,Z)) :-
    value(St, (X,Y,Z)).

% For each intensity I in the image, make
% 255-I in the image output.
%negative :-
%    copy_term([], A_list),
%    matrix(Matrix),
%    coord(Matrix, List),
%    width(List, Size_x),
%    height(List, Size_y),
%    between(0, Size_x, Middle_x),
%        between(0, Size_y, Middle_y),
%            getPixel(List, (Middle_x, Middle_y, Intensity)),
%            New_intensity is 255-Intensity,
%            copy_term([(Middle_x, Middle_y, New_intensity)], Var),
%            append(A_list, Var, Updated),
%        false,
%    true.

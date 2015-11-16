% Image processing package in Prolog
% Credits:
%   Caique Rodrigues Marques
%   Gustavo José Carpeggiani
%   Vinícius Couto Biermann
%
% Based on basic implementation by:
%   Alexandre G. Silva

% EXAMPLE OF ARRAY
% -------------------------

matrix([[4,0,0,0,0,0,0,0,0,0],
        [1,1,0,0,1,9,0,0,0,0],
        [0,1,0,0,1,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,3,0,0,0,1,0,0,0],
        [0,7,1,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,5,1,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0]]).


% ARRAY TO LIST OF COORDINATES
% -------------------------

coordLine([], _, _, []).

coordLine([H|T], Lin, Col, [(Lin,Col,H)|Tm]) :-
    Col1 is Col + 1,
    coordLine(T, Lin, Col1, Tm).

coordAux([], _, _, []) :- !.

coordAux([H|T], Lin, Col, [Hm|Tm]) :-
    Lin1 is Lin + 1,
    coordLine(H, Lin1, Col, Hm),
    coordAux(T, Lin1, Col, Tm).


coord2coord([], C, C).
/*
coord2coord([H|T], C, Coord) :-
    append(C,H,Cx),
    coord2coord(T, Cx, Coord).
*/
coord2coord([H|T], C, Coord) :-
    coord2coord_aux(H, C, D),
    coord2coord(T, D, Coord).

coord2coord_aux([], C, C).
coord2coord_aux([A|B], C, D) :-
    coord2coord_aux(B, [A|C], D).

% Transforms matrix to a coordinates list
%
% Mat = Matrix
% Coord = Coordinates list
coord(Mat, Coord) :-
    coordAux(Mat, -1, 0, CoordMat),
    coord2coord(CoordMat, [], CoordRev),
    reverse(CoordRev, Coord).

% LIST OF COORDINATES TO ARRAY
% -------------------------
% Example: 
%    ?- coord2matrix([(0,0,50),(0,1,10),(0,2,30),(1,0,10),(1,1,20),(1,2,40)], M).
%    M = [[50, 10, 30], [10, 20, 40]].

coord2matrix(S, M) :-
    shape(S, H, W),
    matrixconstruct(S, H, W, -1, [], M),
    !.

matrixconstruct(_, H, _, H, [_|Mt], M) :-
    reverse(Mt, M).
matrixconstruct(S, H, W, L, Macc, M) :-
    L1 is L + 1,
    lineconstruct(S, W, Line, Rest),
    matrixconstruct(Rest, H, W, L1, [Line|Macc], M).

lineconstruct([], _, [], []).
lineconstruct(Rest, 0, [], Rest).
lineconstruct([(_,_,V)|Ta], N, [V|Tb], Rest) :-
    N1 is N - 1,
    lineconstruct(Ta, N1, Tb, Rest).


% DIMENSIONS, VALUE, AND MAXIMUM
% -------------------------
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

maximum(S, M) :-  %maximum for list of coordinates
    findall( V, value(S,(_,_,V)), Lv ),
    max_list(Lv, M).

% IMAGE OF ZEROS
% -------------------------
% zerosAuxLine((3,4),[],Sb).

zerosAuxLine((X,_), S, S) :-
    X < 0,
    !.
zerosAuxLine((_,X), S, S) :-
    X =< 0,
    !.
zerosAuxLine((H,W), Sa, S) :-
    Wa is W - 1,
    zerosAuxLine((H,Wa), [(H,Wa,0)|Sa], S).

zerosAuxSet((X,_), S, S) :-
    X < 0,
    !.
zerosAuxSet((_,X), S, S) :-
    X < 0,
    !.
zerosAuxSet((H,W), Sa, S) :-
    Ha is H - 1,
    zerosAuxLine((Ha,W), [], Sb),
    append(Sb, Sa, Sc),
    zerosAuxSet((Ha,W), Sc, S).

zeros((H,W), S) :-
    zerosAuxSet((H,W), [], S).

% GET|PUT PIXEL
% -------------------------

% Gets the pixel (X, Y, I) of the coordinates list
%
% [...] = Coordinates list.
% (X, Y, V) = Coordinate desired with points
%       in (X, Y) and intensity V.
getPixel([(A,B,V)|_], (X,Y,V)) :-
    A == X,
    B == Y,
    !.
getPixel([_|St], (X,Y,Z)) :-
    getPixel(St, (X,Y,Z)).

% Puts the pixel changed in the coordinates list
%
% (A,B,V) = Coordinate changed with points in (A,B) 
%       and intensity V.
% List1 = Coordinates list with the pixel desired
%       change.
% List2 = Coordinates list with the new pixel (A, B, V).
putPixel(_, [], []) :- 
    !.
putPixel((A,B,V), [(A,B,_)|T1], [(A,B,V)|T2]) :-
    putPixel((A,B,V), T1, T2),
    !.
putPixel((A,B,V), [(Ax,Bx,Vx)|T1], [(Ax,Bx,Vx)|T2]) :-
    Ax \= A,
    putPixel((A,B,V), T1, T2).
putPixel((A,B,V), [(Ax,Bx,Vx)|T1], [(Ax,Bx,Vx)|T2]) :-
    Bx \= B,
    putPixel((A,B,V), T1, T2).

% NEIGHBORHOOD
% -------------------------

% Gets the pixel above of a specific pixel
%
% S = Coordinates list
% (X,Y,_) = Pixel p1 desired 
% (Xa, Y, V) = Pixel above of p1
above(S, (X,Y,_), (Xa,Y,V)) :-
    X > 0,
    Xa is X - 1,
    getPixel(S, (Xa,Y,V)).

% Gets the pixel below of a specific pixel
%
% S = Coordinates list
% (X,Y,_) = Pixel p1 desired 
% (Xa, Y, V) = Pixel below of p1
below(S, (X,Y,_), (Xa,Y,V)) :-
    Xa is X + 1,
    getPixel(S, (Xa,Y,V)).

% Gets the pixel in the left of a specific pixel
%
% S = Coordinates list
% (X,Y,_) = Pixel p1 desired 
% (Xa, Y, V) = Pixel in the left of p1
left(S, (X,Y,_), (X,Ya,V)) :-
    Y > 0,
    Ya is Y - 1,
    getPixel(S, (X,Ya,V)).

% Gets the pixel in the right of a specific pixel
%
% S = Coordinates list
% (X,Y,_) = Pixel p1 desired 
% (Xa, Y, V) = Pixel in the right of p1
right(S, (X,Y,_), (X,Ya,V)) :-
    Ya is Y + 1,
    getPixel(S, (X,Ya,V)).

neighbor(S, (X,Y,V), E) :-
    above(S, (X,Y,V), E).
neighbor(S, (X,Y,V), E) :-
    below(S, (X,Y,V), E).
neighbor(S, (X,Y,V), E) :-
    left(S, (X,Y,V), E).
neighbor(S, (X,Y,V), E) :-
    right(S, (X,Y,V), E).

% Gets the neighbor four of a specific pixel p1, it is, gets
% the pixels above, below, in the right and in the left of
% the pixel p1.
%
% S = Coordinates list
% (X, Y, V) = Pixel p1 desired
% N = List with the neighbor 4 of p1
n4(S, (X,Y,V), N) :-
    findall(E, neighbor(S, (X,Y,V), E), N). 

% TESTS
% -------------------------

% Simple test to get the matrix used, the coordinates list
% and the intensity of the point (0, 0) of the matrix.
%
% M = Matrix
% S = Coordinates list
% V = Intensity desired
test1(M,S,V) :-
    matrix(M), % Get the matrix M
    coord(M,S), % Transform the matrix M to a coordinates list S
    getPixel(S,(0,0,V)).
    % Get the intensity of the pixel in (0, 0)

% Simple test that returns the matrix, the coordinates
% list and the coordinate above of (1, 0), that is, (0, 0) 
%
% M = Matrix
% S = Coordinates list
% V = Intensity
% X = Point above of (1, 0)
test2(M,S,V,X) :-
    matrix(M),
    coord(M,S),
    above(S,(1,0,V),X).
    % Get the point above (1, 0) with intensity V of the
    % list S and stores it in X.

% Simple test that returns the matrix, the coordinates
% list, the intensity and the four neighbors of the
% point (1, 4), that is, the points above ((0,4)), in the
% right ((1,5)), in the left ((1,3)) and below ((2,4)) of 
% (1,4).
%
% M = Matrix
% S = Coordinates list
% V = Intensity
% X = List with 4 neighbors of the point (1, 4)
test3(M,S,V,X) :-
    matrix(M),
    coord(M,S),
    n4(S,(1,4,V),X).
    % In the list S, get the coordinate (1, 4) with intensity
    % V and puts their four neighbors in X.

% Simple test that returns the matrix, the coordinates list,
% the intensity of the pixel (5,1), a new coordinate list
% with the pixel of intensity V1.
%
% M = Matrix
% S = Coordinates list
% V = Intensity
% S1 = New coordinates list with the pixel (5, 1, 777)
% V1 = intensity of the pixel (5, 1)
test4(M,S,V,S1,V1) :-
    matrix(M),
    coord(M,S),
    getPixel(S,(5,1,V)),
    putPixel((5,1,777),S,S1),
    % Puts the pixel (5, 1, 777) of the list S in the new list S1
    getPixel(S1,(5,1,V1)).
    % Gets the intensity of the pixel (5,1) of the list S1

/*

?- test1(M,S,V).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
V = 4.

?- test2(M,S,V,X).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
X = (0, 0, 4).

?- test3(M,S,V,X).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
X = [ (0, 4, 0), (2, 4, 1), (1, 3, 0), (1, 5, 9)].

?- test4(M,S,V,S1,V1).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
V = 7,
S1 = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
V1 = 777

*/

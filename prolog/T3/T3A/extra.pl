% Image processing package in Prolog
% Credits:
%   Caique Rodrigues Marques
%   Gustavo José Carpeggiani
%   Vinícius Couto Biermann
%
% Based on basic implementation by:
%   Alexandre G. Silva
%
% Example: 
%    ?- coord2matrix([(0,0,50),(0,1,10),(0,2,30),(1,0,10),(1,1,20),(1,2,40)], M).
%    M = [[50, 10, 30], [10, 20, 40]].

:- consult('imagem.pl').
:- consult('pgm.pl').

test :-
    load('imgs/ufsc.pgm', S),
    coord2matrix(S, M),
    writePGM('ufsc_out.pgm', M),
    !.

negative(FileName) :-
    %copy_term('rm ', Remove),
    %atom_concat(Remove, FileName, Command),
    %shell(Command, _),
    atom_concat('imgs/', FileName, Path_file),
    print(Path_file), nl, nl,
    load(Path_file, C_list),
    negative_list(C_list, N_list),
    coord2matrix(N_list, Matrix),
    atom_string(FileName, String),
    atomic_list_concat(S_file, '.', String),
    nth0(0, S_file, Name),
    atom_concat(Name, '_negative.pgm', NewFileName),
    writePGM(NewFileName, Matrix),
    !.

mean(FileName1, FileName2) :-
    atom_concat('imgs/', FileName1, Path_f1),
    atom_concat('imgs/', FileName2, Path_f2),
    print(Path_f1), nl,
    print(Path_f2), nl, nl,
    load(Path_f1, L1),
    load(Path_f2, L2),
    mean_list(L1, L2, L_output),
    coord2matrix(L_output, M_output),

    atom_string(FileName1, String1),
    atomic_list_concat(S_file1, '.', String1),
    nth0(0, S_file1, Name1),

    atom_string(FileName2, String2),
    atomic_list_concat(S_file2, '.', String2),
    nth0(0, S_file2, Name2),

    atom_concat(Name1, '_', T_name),
    atom_concat(T_name, Name2, Name_file),
    atom_concat(Name_file, '_mean.pgm', NewFileName),
    writePGM(NewFileName, M_output),
    !.

test_lonely_pixel :-
    matrix(M),
    coord(M, L),
    lonely_pixel(L, O),
    print(O),
    !.

test_path_pixels :-
    matrix(M),
    coord(M, L),
    path_pixels(L, (0, 2, _), (0, 3, _), List),
    print(List),
    !.

load(FileName, S) :-
    readPGM(FileName, M),
    coord(M, S).

% Negative: for each intensity I in the image, make
% 255-I in the image output.
%
% [(X, Y, I)|T_input] = Input coordinates list
% [H_output|T_output] = Output coordinates list
negative_list([], []) :-
    !.
negative_list([(X, Y, I)|T_input], [H_output|T_output]) :-
    New_intensity is 255 - I,
    copy_term((X, Y, New_intensity), H_output),
    negative_list(T_input, T_output).

% Mean between images: each pixel of result image it is
% obtained by the sum of the correspondings pixels of two 
% input images, with the same dimensions, divided by two
% (rounded by the nearest integer).
%
% [(X, Y, I1)|T1] = input coordinates list
% [(_, _, I2)|T2] = input coordinates list
% [H_output|T_output] = output coordinates list
mean_list([], [], []) :-
    !.
mean_list([(X, Y, I1)|T1], [(_, _, I2)|T2], [H_output|T_output]) :-
    Mean_Intensity is (I1 + I2)/2,
    copy_term((X, Y, Mean_Intensity), H_output),
    mean_list(T1, T2, T_output).

% Isolated pixel detector: a pixel with intensity I is
% isolated if your 4 neighbors (below, above, right and left)
% has intensities smallest than I.
%
% [(X, Y, I)|Tail] = Input coordinates list.
% [H_output|T_output] = List with isolated pixels
%
% check_n4_intensity: Check if a pixel is isolated.
%
% [(X, Y, I)|Tail] = List with 4 neighbors of a pixel.
% I_intial = Intensity of pixel that will be compared
%       with the intensities of your 4 neighbors.
check_n4_intensity([], _) :-
    true, !.
check_n4_intensity([(_, _, I)|Tail], I_base) :-
    I_base > I,
    check_n4_intensity(Tail, I_base).

lonely_pixel([], []) :- !.    
lonely_pixel([(X, Y, I)|Tail], [H_output|T_output]) :-
    n4(Tail, (X, Y, I), Four_list),
    (
        check_n4_intensity(Four_list, I) -> 
                copy_term((X, Y, I), H_output),
                lonely_pixel(Tail, T_output);
        lonely_pixel(Tail, [H_output|T_output])
    ).

% Verification of a path between two pixels: There is
% a path between two pixels, if has set of adjacents
% pixels (cosidering the 4 neighbors), all with inten-
% sities bigger or equal then the intensity of the 
% start pixel, that can reach the destiny pixel.
%
% [(X, Y, I)|Tail] = Input coordinates list.
% (Xs, Ys, Is) = Start pixel p1.
% (Xd, Yd, Id) = Destiny pixel p2.
% [H_output|T_output] = List with path from p1 to p2.
%
% get_bigger_intensity: Check the pixel with greater inten-
% sity from the 4 neighbors.
%
% [(X, Y, I)|Tail] = List with 4 neighbors.
% (Xs, Ys, Is) = Pixel with greater intensity.
%
% check_destiny: Check if reached to the destiny pixel.
%
% is_equal: Check if two pixels are equal.
check_destiny((Xs, Ys, _), (Xd, Yd, _)) :-
    Xs = Xd,
    Ys = Yd,
    true.

is_equal((Xs, Ys, _), (Xd, Yd, _)) :-
    Xs = Xd, Ys = Yd, !.

get_bigger_intensity([], (Xs, Ys, Is), (Xd, Yd, Id)) :-
    Xd is Xs,
    Yd is Ys,
    Id is Is.
get_bigger_intensity([(X, Y, I)|Tail], (Xs, Ys, Is), Destiny) :-
    (
        Is =< I ->  get_bigger_intensity(Tail, (X, Y, I), Destiny);
            get_bigger_intensity(Tail, (Xs, Ys, Is), Destiny)
    ).

path_pixels([], _, _, []) :- !.
path_pixels(C_list, (Xs, Ys, _), (Xd, Yd, _), [H_output|T_output]) :-
    getPixel(C_list, (Xs, Ys, Is)),
    getPixel(C_list, (Xd, Yd, Id)),
    copy_term((Xs, Ys, Is), H_output),
    (
        check_destiny((Xs, Ys, Is), (Xd, Yd, Id)) ->
            writeln('Path found!'),
            copy_term([], T_output), 
            write('Path: '),
            true;
        n4(C_list, (Xs, Ys, Is), Four_list),
        get_bigger_intensity(Four_list, (Xs, Ys, Is), Bigger),
        (
            is_equal((Xs, Ys, Is), Bigger) ->
                writeln('Path not found.'), nl,
                copy_term([], T_output),
                write('Actual path: ');
            path_pixels(C_list, Bigger, (Xd, Yd, Id), T_output)
        )
    ).

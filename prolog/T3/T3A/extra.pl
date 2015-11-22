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
%
% Note:
%   For more information about the native rules, check
%   the official documentation for more information at:
%   http://www.swi-prolog.org/

:- consult('imagem.pl').
:- consult('pgm.pl').

test :-
    load('imgs/ufsc.pgm', S),
    coord2matrix(S, M),
    writePGM('ufsc_out.pgm', M),
    !.

% Returns the negative of an ascii pgm image.
% see: negative_list/2
%
% FileName = ascii pgm image
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

% Returns an image with mean of intensities between
% two ascii pgm images.
% see: mean_list/3
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

% Test lonely_pixel rule.
% see: lonely_pixel/4
test_lonely_pixel :-
    matrix(M),
    coord(M, L),
    lonely_pixel(L, L, [], A),
    print(A),
    !.

% Test path_pixels rule
% see: path_pixels/4
%
% (X1, Y1, _) = start pixel
% (X2, Y2, _) = finish pixel
test_path_pixels((X1, Y1, _), (X2, Y2, _)) :-
    load('imgs/ufsc.pgm', L),
    path_pixels(L, (X1, Y1, _), (X2, Y2, _), [], List),
    print(List),
    !.

% Intensifies the dark pixels of an image
% see: get_dark_pixels_image/3
%
% FileName = pgm image
dark_pixels(FileName) :-
    atom_concat('imgs/', FileName, Path_file),
    print(Path_file), nl, nl,
    load(Path_file, C_list),
    get_dark_pixels_image(C_list, [], Dark_list),
    coord2matrix(Dark_list, Matrix),
    atom_string(FileName, String),
    atomic_list_concat(S_file, '.', String),
    nth0(0, S_file, Name),
    atom_concat(Name, '_dark.pgm', NewFileName),
    writePGM(NewFileName, Matrix), !.

% Intensifies the clear pixels of an image.
% see: get_clear_pixels_image/3
%
% FileName = pgm image
clear_pixels(FileName) :-
    atom_concat('imgs/', FileName, Path_file),
    print(Path_file), nl, nl,
    load(Path_file, C_list),
    get_clear_pixels_image(C_list, [], Clear_list),
    coord2matrix(Clear_list, Matrix),
    atom_string(FileName, String),
    atomic_list_concat(S_file, '.', String),
    nth0(0, S_file, Name),
    atom_concat(Name, '_clear.pgm', NewFileName),
    writePGM(NewFileName, Matrix), !.

% Transforms an pgm image to a coordinates list
%
% FileName = pgm image.
% S = Coordinates list.
load(FileName, S) :-
    readPGM(FileName, M),
    coord(M, S).

% Negative: for each intensity I in the image, make
% 255-I in the image output.
% Native rules: copy_term/2
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
% Native rules: copy_term/2, reverse/2.
%
% C_list = input coordinates list
% [(X, Y, I)|Tail] = Remaining coordinates to evaluate.
% T_acc = Accumulator, initially, must be an empty list ([]).
% Output = List with isolated pixels
%
% check_n4_intensity: Check if a pixel is isolated.
%
% [(_, _, I)|Tail] = List with 4 neighbors of a pixel.
% I_base = Intensity of pixel that will be compared
%       with the intensities of your 4 neighbors.
check_n4_intensity([], _) :- true, !.
check_n4_intensity([(_, _, I)|Tail], I_base) :-
    I_base > I,
    check_n4_intensity(Tail, I_base).

lonely_pixel(_, [], T_acc, Output) :-
    reverse(T_acc, Output).
lonely_pixel(C_list, [(X, Y, I)|Tail], T_acc, Output) :-
    n4(C_list, (X, Y, I), Four_list),
    (
        check_n4_intensity(Four_list, I) ->
            lonely_pixel(C_list, Tail, [(X, Y, I)|T_acc], Output);
        lonely_pixel(C_list, Tail, T_acc, Output)
    ).

% Verification of a path between two pixels: There is
% a path between two pixels, if has set of adjacents
% pixels (cosidering the 4 neighbors), all with inten-
% sities bigger or equal then the intensity of the 
% start pixel, that can reach the destiny pixel.
% Native rules: copy_term/2, writeln/1,
%           intersection/3, subtract/3.
%
% C_list = Input coordinates list.
% (Xs, Ys, Is) = Start pixel p1.
% (Xd, Yd, Id) = Destiny pixel p2.
% T_acc = Accumulator, initially, must be an empty list ([]).
% Output = Output list.
%
% get_bigger_intensity: Check the pixel with greater inten-
% sity from the 4 neighbors.
%
% [(X, Y, I)|Tail] = List with 4 neighbors.
% (Xs, Ys, Is) = Pixel with greater intensity.
%
% check_destiny: Check if reached to the destiny pixel.
%
% check_loop: Check if a pixel has been marked.
check_loop([], _) :-
    false.
check_loop([(X, Y, _)|Tail], (Xs, Ys, _)) :-
    (
        X = Xs, Y = Ys ->
            true;
        check_loop(Tail, (Xs, Ys, _))
    ).

check_destiny((Xs, Ys, _), (Xd, Yd, _)) :-
    Xs = Xd,
    Ys = Yd,
    true.

get_bigger_intensity([], (Xs, Ys, Is), (Xd, Yd, Id)) :-
    Xd is Xs,
    Yd is Ys,
    Id is Is.
get_bigger_intensity([(X, Y, I)|Tail], (Xs, Ys, Is), Destiny) :-
    (
        Is =< I ->
            get_bigger_intensity(Tail, (X, Y, I), Destiny);
        get_bigger_intensity(Tail, (Xs, Ys, Is), Destiny)
    ).

path_pixels([], _, _, T_acc, Output) :-
    reverse(T_acc, Output).
path_pixels(C_list, (Xs, Ys, _), (Xd, Yd, _), T_acc, Output) :-
    getPixel(C_list, (Xs, Ys, Is)),
    getPixel(C_list, (Xd, Yd, Id)),
    (
        check_destiny((Xs, Ys, Is), (Xd, Yd, Id)) ->
            writeln('Path found!'),
            reverse([(Xs, Ys, Is)|T_acc], Output),
            true;
        n4(C_list, (Xs, Ys, Is), Four_list),
        intersection(Four_list, T_acc, Intersection),
        subtract(Four_list, Intersection, New_4list),
        get_bigger_intensity(New_4list, (Xs, Ys, Is), Bigger),
        (
            check_loop(T_acc, Bigger) ->
                writeln('Path not found.'),
                reverse([(Xs, Ys, Is)|T_acc], Output);
            path_pixels(C_list, Bigger, (Xd, Yd, Id), [(Xs, Ys, Is)|T_acc], Output)
        )
    ).

% Returns a list of pixels, where the pixels that are not
% darker are intensifies to show in a image the clear areas.
%
% [(X, Y, I)|T_input] = input coordinates list.
% T_acc = Accumulator, initially, must be an empty list.
% Pixel_list = List with pixels, where the clear pixels are
% more evident.
get_clear_pixels_image([], T_acc, Pixel_list) :-
    reverse(T_acc, Pixel_list).
get_clear_pixels_image([(X, Y, I)|T_input], T_acc, Pixel_list) :-
    (
        I > 127 ->
            get_clear_pixels_image(T_input, [(X,Y,255)|T_acc], Pixel_list);
        get_clear_pixels_image(T_input, [(X,Y,I)|T_acc], Pixel_list)
    ).

% Returns a list of pixels, where the pixels that are not
% clear are intensifies to show in a image the dark areas.
%
% [(X, Y, I)|T_input] = input coordinates list.
% T_acc = Accumulator, initially, must be an empty list.
% Pixel_list = List with pixels, where the dark pixels are
% more evident.
get_dark_pixels_image([], T_acc, Pixel_list) :-
    reverse(T_acc, Pixel_list).
get_dark_pixels_image([(X, Y, I)|T_input], T_acc, Pixel_list) :-
    (
        I =< 127 ->
            get_dark_pixels_image(T_input, [(X,Y,0)|T_acc], Pixel_list);
        get_dark_pixels_image(T_input, [(X,Y,I)|T_acc], Pixel_list)
    ).

% Returns the dark pixels of an image. The difference between 
% get_dark_pixels_image is in this rule, only the dark pixels
% are stored in a output list.
get_dark_pixels_list([], T_acc, Dark_list) :-
    reverse(T_acc, Dark_list).
get_dark_pixels_list([(X, Y, I)|T_input], T_acc, Dark_list) :-
    (
        I =< 127 ->
            get_dark_pixels_list(T_input, [(X,Y,I)|T_acc], Dark_list);
        get_dark_pixels_list(T_input, T_acc, Dark_list)
    ).

% Returns the clear pixels of an image. The difference between 
% get_clear_pixels_image is in this rule, only the clear pixels
% are stored in a output list.
get_clear_pixels_list([], T_acc, Clear_list) :-
    reverse(T_acc, Clear_list).
get_clear_pixels_list([(X, Y, I)|T_input], T_acc, Clear_list) :-
    (
        I > 127 ->
            get_clear_pixels_list(T_input, [(X,Y,I)|T_acc], Clear_list);
        get_clear_pixels_list(T_input, T_acc, Clear_list)
    ).

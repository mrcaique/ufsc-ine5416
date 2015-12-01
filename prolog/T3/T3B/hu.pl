% Hu's Invariant Moments in Prolog
% Credits:
%   Caique Rodrigues Marques
%   Gustavo José Carpeggiani
%   Vinícius Couto Biermann
%
% Based on basic implementation by:
%   Prof. A. G. Silva - UFSC
%
% Basic reference:
% https://github.com/shackenberg/Image-Moments-in-Python/blob/master/moments.py
%
% Note:
%   For more information about the native rules, check
%   the official documentation for more information at:
%   http://www.swi-prolog.org/

:- consult('img.pl').

mean_x(S, MeanX) :-
    x_times_image(S, L),
    sum(L, N1),
    sum(S, N2),
    MeanX is N1 / N2.

mean_y(S, MeanY) :-
    y_times_image(S, L),
    sum(L, N1),
    sum(S, N2),
    MeanY is N1 / N2.

% raw or spatial moments
m(S, M00, M01, M10, M02, M20, M12, M21, M03, M30) :-
    sum(S, M00),
    x_times_image(S, L1),
    sum(L1, M01),
    y_times_image(S, L2),
    sum(L2, M10),
    xn_times_image(S, 2, L3),
    sum(L3, M02),
    yn_times_image(S, 2, L4),
    sum(L4, M20),
    y2x_times_image(S, L5),
    sum(L5, M12),
    x2y_times_image(S, L6),
    sum(L6, M21),
    xn_times_image(S, 3, L7),
    sum(L7, M03),
    yn_times_image(S, 3, L8),
    sum(L8, M30).

sum([], 0).
sum([(_,_,V)|T], N) :-
    sum(T, N1),
    N is N1 + V.

x_times_image([], []).
x_times_image([(X,Y,V)|T], L) :-
    x_times_image(T, L1),
    V1 is X*V,
    append([(X,Y,V1)], L1, L).

y_times_image([], []).
y_times_image([(X,Y,V)|T], L) :-
    y_times_image(T, L1),
    V1 is Y*V,
    append([(X,Y,V1)], L1, L).

xn_times_image([], _, []).
xn_times_image([(X,Y,V)|T], N, L) :-
    xn_times_image(T, N, L1),
    V1 is X^N*V,
    append([(X,Y,V1)], L1, L).

yn_times_image([], _, []).
yn_times_image([(X,Y,V)|T], N, L) :-
    yn_times_image(T, N, L1),
    V1 is Y^N*V,
    append([(X,Y,V1)], L1, L).

x2y_times_image([], []).
x2y_times_image([(X,Y,V)|T], L) :-
    x2y_times_image(T, L1),
    V1 is X*X*Y*V,
    append([(X,Y,V1)], L1, L).

y2x_times_image([], []).
y2x_times_image([(X,Y,V)|T], L) :-
    y2x_times_image(T, L1),
    V1 is Y*Y*X*V,
    append([(X,Y,V1)], L1, L).

% central moments
mu(S, MU11, MU02, MU20, MU12, MU21, MU03, MU30) :-
    mean_x(S, MeanX),
    mean_y(S, MeanY),
    x_minus_constant(S, MeanX, L1), y_minus_constant(S, MeanY, L2),
    multiply(L1, L2, L3), multiply(L3, S, L4), sum(L4, MU11),
    multiply(L2, L2, L5), multiply(L5, S, L6), sum(L6, MU02),
    multiply(L1, L1, L7), multiply(L7, S, L8), sum(L8, MU20),
    multiply(L1, L5, L9), multiply(L9, S, L10), sum(L10, MU12),
    multiply(L7, L2, L11), multiply(L11, S, L12), sum(L12, MU21),
    multiply(L2, L5, L13), multiply(L13, S, L14), sum(L14, MU03),
    multiply(L1, L7, L15), multiply(L15, S, L16), sum(L16, MU30).

x_minus_constant([], _, []).
x_minus_constant([(X,Y,_)|T], C, L) :-
    x_minus_constant(T, C, L1),
    V1 is X - C,
    append([(X,Y,V1)], L1, L).

y_minus_constant([], _, []).
y_minus_constant([(X,Y,_)|T], C, L) :-
    y_minus_constant(T, C, L1),
    V1 is Y - C,
    append([(X,Y,V1)], L1, L).

sum_constant([], _, []).
sum_constant([(X,Y,V)|T], C, L) :-
    sum_constant(T, C, L1),
    V1 is V + C,
    append([(X,Y,V1)], L1, L).

multiply([], [], []).
multiply([(X,Y,V1)|T1], [(X,Y,V2)|T2], L) :-
    multiply(T1, T2, L1),
    V3 is V1 * V2,
    append([(X,Y,V3)], L1, L).

nu(S, NU11, NU12, NU21, NU02, NU20, NU03, NU30) :-
    sum(S, SUM),
    mu(S, MU11, MU02, MU20, MU12, MU21, MU03, MU30),
    NU11 is MU11 / SUM^(2/2+1),
    NU12 is MU12 / SUM^(3/2+1),
    NU21 is MU21 / SUM^(3/2+1),
    NU02 is MU02 / SUM^(2/2+1),
    NU20 is MU20 / SUM^(2/2+1),
    NU03 is MU03 / SUM^(3/2+1),
    NU30 is MU30 / SUM^(3/2+1).

hu(S, I1, I2, I3, I4, I5, I6, I7) :-
    nu(S, NU11, NU12, NU21, NU02, NU20, NU03, NU30),
    I1 is NU20 + NU02,
    I2 is (NU20 - NU02)^2 + 4*NU11^2,
    I3 is (NU30 - 3*NU12)^2 + (3*NU21 - NU03)^2,
    I4 is (NU30 + NU12)^2 + (NU21 + NU03)^2,
    I5 is (NU30 - 3*NU12) * (NU30 + NU12) * ( (NU30 + NU12)^2
        - 3*(NU21 + NU03)^2 ) + (3*NU21 - NU03) * (NU21 + NU03)
        * ( 3*(NU30 + NU12)^2 - (NU21 + NU03)^2 ),
    I6 is (NU20 - NU02) * ( (NU30 + NU12)^2 - (NU21 + NU03)^2 )
        + 4 * NU11 * (NU30 + NU12) * (NU21 + NU03),
    I7 is (3*NU21 - NU03) * (NU30 + NU12) * ( (NU30 + NU12)^2
        - 3*(NU21 + NU03)^2 ) - (NU30 - 3*NU12) * (NU21 + NU03)
        * ( 3*(NU30 + NU12)^2 - (NU21 + NU03)^2 ).

test :-
    coord([[20,5,1],[4,10,50],[4,2,5]], S),
    writeln(S),
    hu(S, I1, I2, I3, I4, I5, I6, I7),
    writeln(I1),
    writeln(I2),
    writeln(I3),
    writeln(I4),
    writeln(I5),
    writeln(I6),
    writeln(I7).

%% -------------------------------------------------------------------------------------------

% Loads the database.
load :-
    retractall(img(_, _, _, _, _, _, _, _)),
    open('imgdatabase.pl', read, Stream),
    repeat,
        read(Stream, Data),
        (Data == end_of_file -> true ; assert(Data), fail),
        !,
        close(Stream).

% Saves the content modified in the database, that is,
% in the imgdatabase.pl file.
commit :-
    open('imgdatabase.pl', write, Stream),
    telling(Screen),    
    tell(Stream),
    listing(img/8),
    tell(Screen),
    close(Stream).

% Add an element to the database.
new(FileName, Id) :-
    readPGM(FileName, I),
    coord(I, Iout),
    hu(Iout, I1, I2, I3, I4, I5, I6, I7),
    assertz(img(Id, I1, I2, I3, I4, I5, I6, I7)),
    !.

% List the current contents of the database.
search_all(Id) :-
    listing(img(Id, _, _, _, _, _, _, _)).


% Returns the Euclidean distance between the input image and one image from the database.
% Native rule: copy_term/2
%
%%%%% Parameters %%%%%%
% [Input_Head|Input_Tail] is the input image Hu moments, 
% [Data_Head|Data_Tail] is the database image Hu moments, 
% [Output_Head|Output_Tail] is the return list.
%
%%%%%   Trivia   %%%%%%
% -Steve lives!
% -SHAZAM! [⚡BOOOM⚡]
%   - Shazam is the alter ego of Billy Batson, a boy who, by speaking the magic
%     word "Shazam", can transform himself into a costumed adult with the powers
%     of superhuman strength, speed, flight, and other abilities.
%       Shazam (before Captain Marvel) is a superhero created by C. C. Beck and 
%     Bill Parker, first appeared in Whiz Comics #2 (february 1940).
euclidean_dist([], [], []) :- !.

euclidean_dist([Input_Head|Input_Tail], [Data_Head|Data_Tail], [Output_Head|Output_Tail]) :-
    Shazam is Input_Head - Data_Head,
    Steve is Shazam^2,
    copy_term(Steve, Output_Head),
    euclidean_dist(Input_Tail, Data_Tail, Output_Tail).

% Compare the input image with all the images on the database by calculating its 
% Euclidean distance.
% Native rules: sum_list/2, copy_term/2, sqrt/1
%
%%%%% Parameters %%%%%%
% I1 to I7 are the Hu moments of the input image,
% [Data_Head|Data_tail] is the list images of the database,
% [Output_Head|Output_Tail] is the result of the compare
%
%%%%%   Trivia   %%%%%%
% Gandalf is OP.
%   - Gandalf is a Istari (or a messenger) and a mage sent by the Valar to the
%     Middle-earth to organize and communicate the people to prepare against the
%     Sauron, the new Dark Lord.
%     This is from The Lord of the Rings series (1954-1955) by J. R. R. Tolkien.
compare_images(_, _, _, _, _, _, _, [], []) :- !.

compare_images(I1, I2, I3, I4, I5, I6, I7, [Data_Head|Data_Tail], [Output_Head|Output_Tail]) :-
    nth0(0, Data_Head, Data_Class),
    nth0(1, Data_Head, K1),
    nth0(2, Data_Head, K2),
    nth0(3, Data_Head, K3),
    nth0(4, Data_Head, K4),
    nth0(5, Data_Head, K5),
    nth0(6, Data_Head, K6),
    nth0(7, Data_Head, K7),
    img(Data_Class, K1, K2, K3, K4, K5, K6, K7),
    euclidean_dist([I1, I2, I3, I4, I5, I6, I7], [K1, K2, K3, K4, K5, K6, K7], List),
    sum_list(List, Sum),
    OP is sqrt(Sum),
    Gandalf is OP,
    copy_term(Gandalf, Output_Head),
    compare_images(I1, I2, I3, I4, I5, I6, I7, Data_Tail, Output_Tail).

% Checks the image receveid and asks to the user if the conclusion is the
% of the machine is the correct image. if "no", he asks to the user what
% is the correct answer and add to your "knowledge base", as if he has
% learned. If "yes" he checks if is the same image that he seen before,
% if not, he add it to your "knowledge base" with the same name that he
% suggested.
% Native rules: findall/3, min_list/2, nth0/3, write/1, writeln/1,
%   print/1.
%
%%%%% Parameters %%%%%%
% FileName is the path to the an ascii pgm image.
%
%%%%%   Trivia   %%%%%%
% - Padawan is a disciple of a Jedi, from Star Wars saga by George Lucas.
% - First quote after "no" condition (modified) by Aristotle
% - Second quote after "no" condition (modified) by Khalil Gibran
scan_image(FileName) :-
    writeln(FileName),
    writeln('Note: Don\'t forget to end yours answers with a dot (.)'), nl,
    readPGM(FileName, File),
    coord(File, FileCoord),
    hu(FileCoord, I1, I2, I3, I4, I5, I6, I7),
    findall([Data_image, K1, K2, K3, K4, K5, K6, K7], img(Data_image, K1, K2, K3, K4, K5, K6, K7), Data_List),
    compare_images(I1, I2, I3, I4, I5, I6, I7, Data_List, Compare_Out),
    min_list(Compare_Out, Minimal),
    nth0(Index, Compare_Out, Minimal),
    nth0(Index, Data_List, Image),
    nth0(0, Image, Image_Print),
        write('Minimal value found: '), print(Minimal), nl,
        write('Image found: '), print(Image_Print), nl,
        write('Position: '), print(Index), nl, nl,
        writeln('This is your image, young padawan? [y./n.]'),
    read(X),
    (
        (X = 'n' ; X = 'no') ->
            write('Name thy image: '),
            read(Name),
            insert_image(Name, I1, I2, I3, I4, I5, I6, I7),
            writeln('All men and machine by nature desire knowledge.'),
            writeln('Wow, perplexity is the beginning of knowledge!'),
            writeln('Very good!'),
            commit, !;
        !
    ),
    (
        (X = 'y' ; X = 'yes') ->
            (
                (Minimal =:= 0) ->
                    writeln('Thy image is already in the database.'), !;
                insert_image(Image_Print, I1, I2, I3, I4, I5, I6, I7),
                writeln('The same in a new perspective!'),
                writeln('Good!'),
                commit, !
            )
    ).

insert_image(Id, I1, I2, I3, I4, I5, I6, I7) :-
    assertz(img(Id, I1, I2, I3, I4, I5, I6, I7)), !.

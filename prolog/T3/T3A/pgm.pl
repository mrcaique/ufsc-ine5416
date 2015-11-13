% Image processing package in Prolog (a initial tentative)
% Prof. A. G. Silva - UFSC - October 2015
%
% Example: 
%    ?- readPGM('ufsc.pgm', M), writePGM('ufsc_out.pgm', M).


:- use_module(library(pio)).

%---------------------------------------------------
% Write a PGM (text mode by line) image file format

writePGM(FileName, I) :-
    open(FileName, write, File),
    dimensions(I, H, W),
    write(File, 'P2\n'),
    write(File, '# Image Processing in Prolog - UFSC 2015\n'),
    write(File, W), write(File, ' '), write(File, H), write(File, '\n'),
    write(File, '255\n'),
    write_elements(File, I),
    close(File).

write_elements(_, []) :-
    !.
write_elements(File, [H|T]) :-
    write_line(File, H),
    write(File, '\n'),
    write_elements(File, T).

write_line(_, []) :-
    !.
write_line(File, [H|T]) :-
    write(File, H), write(File, ' '),
    write_line(File, T).

dimensions([Ih|It], H, W) :-
    length(Ih, W),
    length(It, H1),
    H is H1 + 1.

%---------------------------------------------------
% Read a PGM (text mode by line) image file format

readPGM(FileName, I) :-
    phrase_from_file(lines(Ls), FileName),
    line_parser(Ls, [], L),
    K is 4,
    remove_elements(L, K, I).  % remove the first K elements (lines)

remove_elements(L, 0, L) :-
    !.
remove_elements([_|T], K, I) :-
    K1 is K - 1,
    remove_elements(T, K1, I).

line_parser([], L, Ls) :-
    reverse(L, Ls).
line_parser([H|T], Z, L) :-
    substitute("   ", " ", H, B1),
    substitute("  ", " ", B1, B),
    atom_codes(C, B),
    atomic_list_concat(D, ' ', C),
    atomic_list_number(D, [], E),
    line_parser(T, [E|Z], L),
    !.

atomic_list_number([], L, Ls) :-
    reverse(L, Ls).
atomic_list_number([A|Ar], Br, L) :-
    atomic_list_number_aux(A, B),
    atomic_list_number(Ar, [B|Br], L).

atomic_list_number_aux(A, B) :-
    catch(atom_number(A, B), _, fail).
atomic_list_number_aux(A, A).

%---------------------------------------------------

eos([], []).

replace(_, _) --> call(eos), !.
replace(Find, Replace), Replace -->
        Find,
        !,
        replace(Find, Replace).
replace(Find, Replace), [C] -->
        [C],
        replace(Find, Replace).

substitute(Find, Replace, Request, Result):-
        phrase(replace(Find, Replace), Request, Result).    

remove([],_,[]) :- !. 
remove([X|T],X,L1) :- !, remove(T,X,L1).         
remove([H|T],X,[H|L1]) :- remove(T,X,L1).

%---------------------------------------------------

lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

%eos([], []).

line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

%---------------------------------------------------
% References:
% http://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

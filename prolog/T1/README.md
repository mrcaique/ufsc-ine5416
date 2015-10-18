## T1
### Part A
The database, in SWI-Prolog, corresponds a curricular grade of the computer science course in the Federal University of Santa Catarina (ufsc.br).

The terms are defined in phases, disciplines and dependencies, each of which is composed as follows:
```prolog
* fase(f[number], [list of disciplines])  % phase
* disciplina(code, name, phase)           % discipline
* depende(chosen discipline, dependency)  % dependency
```
Notes:

* ***1)*** The curricular grid is based on 2007/2 curriculum;
* ***2)*** The optional disciplines are such that phase = 0;
* ***3)*** Some optional disciplines can't be present in a semester.

### Part B
The rules corresponds on the first implementation based on the database and the motivation is start to explore the database, formulating rules for consultations to be held.

The rules are defined as:
```prolog
Head :- Body
```
Corresponds a basic formula in the predicate logic: if "p" then "q". The condition is satisfied if the body's content is true, then the content of the head also is true. Including, based on the facts set out in the database, you can find possible solutions to a given imput set to "Head".

### Part C
The rules corresponds to the second implementation based on the database, where it makes use of lists and of native functions of the SWI-Prolog.

---
## Credits

* Caique Marques ([mrcaique](https://github.com/mrcaique))
* Gustavo José Carpeggiani
* Vinícius Couto Biermann ([ViniciusBiermann](https://github.com/ViniciusBiermann))

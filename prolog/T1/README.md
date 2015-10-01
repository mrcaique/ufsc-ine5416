### T1
The database, in SWI-Prolog, corresponds a curricular grade of the computer science course in the Federal University of Santa Catarina (ufsc.br).

The terms are defined in phases, disciplines and dependencies, each of which is composed as follows:
```prolog
* fase(f[number])                         % phase
* disciplina(code, name, phase)           % discipline
* depende(chosen discipline, dependency)  % dependency
```
Notes:

* ***1)*** The curricular grid is based on 2007/2 curriculum;
* ***2)*** The optional disciplines are such that phase = 0;
* ***3)*** Some optional disciplines can't be present in a semester.

---
### Credits

* Caique Marques ([mrcaique](https://github.com/mrcaique))
* Gustavo José Carpeggiani
* Vinícius Couto Biermann ([ViniciusBiermann](https://github.com/ViniciusBiermann))

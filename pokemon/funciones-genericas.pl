
%-----------------------GENERICAS--------------------------

% --- Imprime cada elemento de lista con su indice, renglon por renglon
cambiarHecho(HechoABorrar,NuevoHecho):-
  retractall(HechoABorrar),
  assert(NuevoHecho).

desplegarOpciones(Lista):-desplegarOpciones(Lista,0).
desplegarOpciones([],_).
desplegarOpciones([Cabeza|Cola],Contador):-
  write(Contador),write("-"),write(Cabeza),nl,
  NuevoContador is Contador + 1,
  desplegarOpciones(Cola,NuevoContador).

% --- regresa un elemento random de una lista

elementoRandomLista(Lista,Elemento):-
  length(Lista,Longitud),
  random(0,Longitud,Indice),
  nth0(Indice,Lista,Elemento).


% --- dada una lista, despliega elementos en forma de opciones y regresa el elemento elegido por el usuario

elegirOpcion(Mensaje,Opciones,ElementoElegido):-
  write(Mensaje),nl,nl,
  desplegarOpciones(Opciones),
  leerOpcion(Opciones,ElementoElegido).
% sin mensaje
elegirOpcion(Opciones,ElementoElegido):-
  leerOpcion(Opciones,ElementoElegido),nl.

elegirOpcionSinValidar(Mensaje,Opciones,ElementoElegido):-
  write(Mensaje),nl,
  desplegarOpciones(Opciones),
  leerOpcion(IndiceOpcion),
  nth0(IndiceOpcion,Opciones,ElementoElegido).


% --- Busca un elemento(E) en a lista, si lo encuentra lo remplaza con NE

remplazarElemento(_,_,[],[]).
remplazarElemento(E,NE,[E|C],[NE|LR]):-remplazarElemento(E,NE,C,LR). %remplaza elemento
remplazarElemento(E,NE,[H|C],[H|LR]):-
  H\=E,
  remplazarElemento(E,NE,C,LR),!. %copia elemento

% --- Remplaza el valor del inidice I de la lista, con el elemento E

remplazarIndice(0,E,[_|C],[E|C]).
remplazarIndice(I,E,[H|C],[H|LR]):-
  NI is I - 1,
  remplazarIndice(NI,E,C,LR),!.

esEnter(13).

esperarRespuesta :-
  get_single_char(_),
  write("Presiona enter para continuar"),nl,
  esperarRespuestaValidada.
esperarRespuesta1Enter:-
  write("Presiona enter para continuar"),nl,
  esperarRespuestaValidada.
esperarRespuestaValidada:-
  get_single_char(C),esEnter(C).
esperarRespuestaValidada:-esperarRespuestaValidada.


leer(Entrada):-
  catch(read(Entrada), _ , false),
  tieneValor(Entrada).

leerOpcion(IndiceOpcion):-
  catch((
    get_single_char(X),
    char_code(Str,X),
    atom_number(Str,IndiceOpcion)
  ), _ , false),
  tieneValor(IndiceOpcion).

leerOpcion(Opciones,ElementoElegido):-
  leerOpcion(IndiceOpcion),
  nth0(IndiceOpcion,Opciones,ElementoElegido).
leerOpcion(Opciones,Entrada):-leerOpcion(Opciones,Entrada).

tieneValor(X):-not(var(X)).

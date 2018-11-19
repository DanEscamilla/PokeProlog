:-ensure_loaded("funciones-genericas.pl").
:-ensure_loaded("mapa-global.pl").
:-ensure_loaded("mapa-ciudad.pl").
:-dynamic
  jugadorSeMovio/0,
  tipoTransicion/1,
  transicion/1,
  mapa/2,
  objetoEnMapa/4,
  posicionJugador/2,
  posicionVista/2.

posicionJugador(1,1).

movimiento("Derecha",1,0).
movimiento("Izquierda",-1,0).
movimiento("Abajo",0,1).
movimiento("Arriba",0,-1).
codigoADireccion(100,"Derecha").
codigoADireccion(97,"Izquierda").
codigoADireccion(115,"Abajo").
codigoADireccion(119,"Arriba").
codigoADireccion(50,"Transicion Entrada").
codigoADireccion(49,"Transicion Salida").
codigoADireccion(27,"Terminar el programa").

vista(30,30).
posicionVista(0,0).

generarObjetos([]).
generarObjetos([[DescVisual,X,Y,Nombre]|ObjetosRestantes]):-
  assert(objetoEnMapa(DescVisual,X,Y,Nombre)),
  generarObjetos(ObjetosRestantes).


abrirMapa:-
  posicionJugador(JugadorX,JugadorY),
  moverPosicionVista(JugadorX,JugadorY),
  correr.

correr:-
  shell(clear),nl,
  moverse.

moverse:-
  imprimirMapa,
  elegirOpcionMoverse(Opcion),
  accionOpcion(Opcion).

accionOpcion("Terminar el programa"):-
  transicionSalida,
  abort.
accionOpcion(Direccion):-
  moverJugador(Direccion),
  correr.

moverJugador(Direccion):-
  movimiento(Direccion,IncrementoX,IncrementoY),
  posicionJugador(JugadorX,JugadorY),
  NuevaY is JugadorY + IncrementoY,
  NuevaX is JugadorX + IncrementoX,
  validarMovimiento(NuevaX,NuevaY),
  moverJugador(NuevaX,NuevaY).

moverJugador(NuevaX,NuevaY):-
  cambiarHecho(posicionJugador(_,_),posicionJugador(NuevaX,NuevaY)),
  moverPosicionVista(NuevaX,NuevaY),
  mandarCollisionConObjeto(NuevaX,NuevaY),
  jugadorSeMovio.

mandarCollisionConObjeto(NuevaX,NuevaY):-
  objetoEnMapa(DescObjeto,NuevaX,NuevaY,NombreObjeto),
  collisionMapa(DescObjeto,NuevaX,NuevaY,NombreObjeto).
mandarCollisionConObjeto(_,_).

collisionMapa(_,_,_,_).
  % nl,write(" Chocaste con "),write(NombreObjeto),nl,
  % abort.

validarMovimiento(X,Y):-
  not(hayPared(X,Y)),
  estaDentroDeMapa(X,Y).

estaDentroDeMapa(X,Y):-
  mapa(Anchura,Altura),
  AlturaLimite is Altura -1,
  X>0,X=<Anchura,
  Y>0,Y=<AlturaLimite.

hayPared(X,Y):-objetoEnMapa(_,X,Y,"wall"). %pared horizontal
hayPared(X,Y):-objetoEnMapa(_,X,Y,""). %pared vertical

moverPosicionVista(JugadorX,JugadorY):-
  vista(AnchuraVista,AlturaVista),
  mapa(AnchuraMapa,AlturaMapa),
  moverPosicionVistaHorizontal(JugadorX,AnchuraVista,AnchuraMapa),
  moverPosicionVistaVertical(JugadorY,AlturaVista,AlturaMapa).


moverPosicionVistaVertical(JugadorY,AlturaVista,AlturaMapa):-
  MitadVertical is AlturaVista/2,
  LimiteInferiror is AlturaMapa - AlturaVista,
  VistaY is JugadorY-MitadVertical,
  VistaYAjustada is max(0,min(LimiteInferiror,VistaY)),
  actualizarYVista(VistaYAjustada).
moverPosicionVistaVertical(_,_,_).

moverPosicionVistaHorizontal(JugadorX,AnchuraVista,AnchuraMapa):-
  MitadHorizontal is AnchuraVista/2,
  LimiteDerecho is AnchuraMapa - AnchuraVista,
  VistaX is JugadorX-MitadHorizontal,
  VistaXAjustada is max(0,min(LimiteDerecho,VistaX)),
  actualizarXVista(VistaXAjustada).
moverPosicionVistaHorizontal(_,_,_).


actualizarXVista(NuevaVistaX):-
  posicionVista(_,VistaY),
  cambiarHecho(posicionVista(_,_),posicionVista(NuevaVistaX,VistaY)).

actualizarYVista(NuevaVistaY):-
  posicionVista(VistaX,_),
  cambiarHecho(posicionVista(_,_),posicionVista(VistaX,NuevaVistaY)).

ajustarPosicionAVista(X,Y,XAjustada,YAjustada):-
  posicionVista(VistaX,VistaY),
  XAjustada is X + VistaX,
  YAjustada is Y + VistaY.





% ------------ ImprimirMapa -------------


imprimirMapa:-
  vista(Anchura,Altura),
  tab(48),write("Mapa"),nl,
  imprimirLineaArriba(Anchura),
  forall(between(1,Altura,NumeroFila),imprimirFila(NumeroFila,Anchura)),nl,
  imprimirPosicion,nl.

imprimirPosicion:-
  posicionJugador(X,Y),
  tab(6),write("Tu posicion:"),nl,
  tab(6),write("x:"),write(X),write(", y:"),write(Y),nl.

imprimirLineaArriba(Anchura):-
  tab(6),
  forall(between(1,Anchura,NumeroColumna),imprimirLinea(NumeroColumna)),nl.
imprimirLinea(NumeroColumna):-manejarTransicion(NumeroColumna).
imprimirLinea(_):-write("___").

imprimirLimiteIzquierdo:-manejarTransicion(0).
imprimirLimiteIzquierdo:-write("|").
imprimirLimiteDerecho:-
  vista(AnchuraVista,_),
  manejarTransicion(AnchuraVista).
imprimirLimiteDerecho:-write("|").

imprimirFila(NumeroFila,Anchura):-
  tab(5),imprimirLimiteIzquierdo,
  forall(between(1,Anchura,NumeroColumna),imprimirCelda(NumeroColumna,NumeroFila)),
  imprimirLimiteDerecho,nl.

imprimirCelda(NumeroColumna,NumeroFila):-
  ajustarPosicionAVista(NumeroColumna,NumeroFila,XEnVista,YEnVista),
  imprimirCeldaAjustada(XEnVista,YEnVista).

imprimirCeldaAjustada(NumeroColumna,_):-manejarTransicion(NumeroColumna).
imprimirCeldaAjustada(XEnVista,YEnVista):-
  posicionJugador(XEnVista,YEnVista),
  write("Tu ").

imprimirCeldaAjustada(XEnVista,YEnVista):-
  objetoEnMapa(DescVisual,XEnVista,YEnVista,_),
  string_chars(DescVisual,[L1,L2,L3|_]), %primeras 3 letras del nombre
  write(L1),write(L2),write(L3).

imprimirCeldaAjustada(_,YEnVista):-
  posicionVista(_,VistaY),
  Y is YEnVista - VistaY,
  vista(_,AlturaVista),
  Y = AlturaVista,
  write("___").
imprimirCeldaAjustada(_,_):-write("   ").

elegirOpcionMoverse(ElementoElegido):-
  write("Utiliza las w/a/s/d para moverte"),nl,
  leerDireccion(ElementoElegido).

leerDireccion(ElementoElegido):-
    get_single_char(Codigo),
    codigoADireccion(Codigo,ElementoElegido).
leerDireccion(ElementoElegido):-leerDireccion(ElementoElegido).




%  -------------------- Transiciones -------------------



transicionSalida:-
  cambiarHecho(tipoTransicion(_),tipoTransicion(salida)),
  transicionando,
  terminarTransicionSalida.
transicionEntrada:-
  cambiarHecho(tipoTransicion(_),tipoTransicion(entrada)),
  transicionando,
  terminarTransicionEntrada.

transicionando:-
  vista(Anchura,_),
  assert(transicion(0)),
  AnchuraMitad is round((Anchura + 1)/2),
  forall(between(1,AnchuraMitad,NumeroColumna),
  (
    NumeroColumnaMultiplicada is NumeroColumna * 2,
    shell(clear),nl,
    imprimirMapa,
    sleep(0.05),
    cambiarHecho(transicion(_),transicion(NumeroColumnaMultiplicada))
  )).

manejarTransicion(NumeroColumna):-
  tipoTransicion(salida),
  manejarTransicionSalida(NumeroColumna).

manejarTransicion(NumeroColumna):-
  tipoTransicion(entrada),
  manejarTransicionEntrada(NumeroColumna).



manejarTransicionSalida(NumeroColumna):-
  transicion(ColumnaTransicion),
  NumeroColumna<ColumnaTransicion,
  write("|||").
manejarTransicionSalida(NumeroColumna):-
  transicion(ColumnaTransicion),
  NumeroColumna=ColumnaTransicion,
  write("|||").

manejarTransicionEntrada(NumeroColumna):-
  transicion(ColumnaTransicion),
  NumeroColumna>ColumnaTransicion,
  write("|||").
manejarTransicionEntrada(NumeroColumna):-
  transicion(ColumnaTransicion),
  NumeroColumna=ColumnaTransicion,
  write("|||").


terminarTransicionSalida:-terminarTransicion.
terminarTransicionEntrada:-
  terminarTransicion,
  correr.

terminarTransicion:-
  shell(clear),
  retractall(tipoTransicion(_)),
  retractall(transicion(_)).

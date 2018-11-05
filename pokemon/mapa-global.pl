listaObjetosEnMapaGlobal([
  ["Paleta",2,2],
  ["Verde",5,5]
]).

posicionJugadorGlobal(1,1).

definirMapaGlobal:-
  retractall(transicion(_)),
  cambiarHecho(mapa(_,_),mapa(100,100)),
  posicionJugadorGlobal(X,Y),
  cambiarHecho(posicionJugador(_,_),posicionJugador(X,Y)),
  cambiarHecho(jugadorSeMovio,(jugadorSeMovio:-jugadorSeMovioAdaptador)),
  retractall(objetoEnMapa(_,_,_)),
  listaObjetosEnMapaGlobal(ObjetosMapaGlobal),
  generarObjetos(ObjetosMapaGlobal).

generarObjetos([]).
generarObjetos([[Nombre,X,Y]|ObjetosRestantes]):-
  assert(objetoEnMapa(Nombre,X,Y)),
  generarObjetos(ObjetosRestantes).

abrirMapaGlobal:-
  definirMapaGlobal,
  abrirMapa.

posibilidadesCamino:-
  random(RandomNum),
  posibilidadCamino(RandomNum).

posibilidadCamino(RandomNum):-RandomNum<0.9. % 90% que no pase nada
posibilidadCamino(RandomNum):-
  RandomNum<0.91, %  10% de probabilidad de que el encuentro sea un pokehuevo
  encontrarPokehuevo.
posibilidadCamino(RandomNum):-
  RandomNum<0.92, %  10% de probabilidad de que el encuentro sea una pokebola
  encontrarPokebola.
posibilidadCamino(RandomNum):-
  RandomNum<0.98, %  50% de probabilidad de que el encuentro sea un pokemon
  encontrarPokemon.
posibilidadCamino(RandomNum):-
  RandomNum=<1, %  20% de probabilidad de que el encuentro sea un entrenador
  encontrarEntrenador.

jugadorSeMovioAdaptador:-
  posibilidadesCamino,
  actualizarPokehuevo.

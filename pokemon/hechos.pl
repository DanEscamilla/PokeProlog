:-dynamic
  ciudadActual/1,
  pokehuevosEntrenador/1,
  pokemonesEntrenador/1,
  medallasEntrenador/1,
  dineroEntrenador/1,
  pokebolasEntrenador/1,
  pokemonActivo/1,
  pokemonEnemigo/1,
  pokemonesBill/1,
  pokehuevosBill/1,
  peleandoCon/1,
  liderGimnasio/2,
  entrenadorActivo/2,
  habilidad/3,
  distanciaEntreCiudades/3.

% BASE DE CONOCIMIENTO ESTATICA

  % nombre, distancia necesaria para eclocionar, distancia recorrida
pokehuevos([[electricidad,190],[planta,170],[fuego,160],[agua,140],[piedra,120]]).
  % nombre,costo,probabilidad,
pokebolas([[normal,10,0.5],[super,25,0.7],[master,50,1]]).
  % nombre, tipo, vida,vidaActual, estado, experiencia, nivel, ataques, (NombrePersonalizado)
pokemones([
%electricidad
  ["pikachu",electricidad,120,120,excelente,0,1,["Impactrueno","Ataque Rapido","Chispa","Voltio cruel"]],
  ["magnemite",electricidad,120,120,excelente,0,1,["Bomba sonica","Doble rayo","Trueno","Bola voltio"]],
  ["electabuzz",electricidad,120,120,excelente,0,1,["Impactrueno","Rayo","Golpe Trueno","Patada baja"]],
%planta
  ["bulbasaur",planta,120,120,excelente,0,1,["Latigo cepa","Polvo veneno","Doble filo","Hoja afilada"]],
  ["bellsprout",planta,120,120,excelente,0,1,["Somnifero","Paralizador","Atizar","Acido"]],
  ["tangela",planta,120,120,excelente,0,1,["Restriccion","Atadura","Desarme","Latigazo"]],
%fuego
  ["charmander",fuego,120,120,excelente,0,1,["Lanzallamas","Onda ignea","Llamarada","Mordisco"]],
  ["ponyta",fuego,120,120,excelente,0,1,["Nitrocarga","Derribo","Giro fuego","Llamarada"]],
  ["magmar",fuego,120,120,excelente,0,1,["Golpe fuego","Lanzallamas","Pirotecnia","Ascuas"]],
%agura
  ["squirtle",agua,120,120,excelente,0,1,["Coletazo","Cascada","Pistola de agua","Burbuja"]],
  ["staryu",agua,120,120,excelente,0,1,["Giro rapido","Hidropulso","Pistola de agua","Rayo burbuja"]],
  ["lapras",agua,120,120,excelente,0,1,["Hidrocanon","Salpicar","Torbellino","Ventisca"]],
%piedra
  ["geodude",piedra,120,120,excelente,0,1,["Lanzarrocas","Golpe roca","Demolucion","Pedrada"]],
  ["rhyhorn",piedra,120,120,excelente,0,1,["Megacuerno","Pataleta","Avalancha","Taladradora"]],
  ["onix",piedra,120,120,excelente,0,1,["Roca afilada","Terremoto","Avalancha","Tumbar rocas"]]
]).
pokemonesEspeciales([
  ["articuno",agua,1000,1000,excelente,0,1,["Rayo hielo","Ciclon","Tornado","Hiperrayo"]],
  ["zapdos",electricidad,1000,1000,excelente,0,1,["Electrocanon","Ciclon","Aire afilado","Hiperrayo"]],
  ["moltres",fuego,1000,1000,excelente,0,1,["Nitrocarga","Llamarada final","Calcinacion","Hiperrayo"]],
  ["mewtwo",psiquico,2500,2500,excelente,0,1,["Poder1","Poder2","Poder3","Poder4"]]
]).

% relacion entre ataques y que tipo son.
tipoAtaques(piedra,["Lanzarrocas","Golpe roca","Pedrada","Avalancha","Taladradora","Roca afilada","Terremoto","Avalancha","Tumbar rocas"]).
tipoAtaques(agua,["Cascada","Pistola de agua","Burbuja","Hidropulso","Pistola de agua","Rayo burbuja","Hidrocanon","Salpicar","Ventisca","Rayo hielo","Ciclon"]).
tipoAtaques(fuego,["Llamarada final","Calcinacion","Lanzallamas","Onda ignea","Llamarada","Nitrocarga","Giro fuego","Golpe fuego","Pirotecnia"]).
tipoAtaques(planta,["Latigo cepa","Polvo veneno","Somnifero","Paralizador","Acido","Latigazo"]).
tipoAtaques(electricidad,["Electrocanon","Impactrueno","Chispa","Voltio cruel","Trueno","Bola voltio","Doble rayo","Rayo","Golpe Trueno"]).

%Evoluciones
% pokemon original, evolucion, nivel requerido.
evoluciones("pikachu","raichu",2).
evoluciones("magnamite","magneton",2).

evoluciones("charmander","charmeleon",2).
evoluciones("charmeleon","charizard",3).
evoluciones("ponyta","rapidash",2).

evoluciones("squirtle","warturtle",2).
evoluciones("warturtle","blastoise",3).
evoluciones("staryu","starmite",2).

evoluciones("bulbasaur","ivysaur",2).
evoluciones("ivysaur","vinasaur",3).
evoluciones("bellsprout","weepinbell",2).
evoluciones("weepinbell","victreebel",3).

evoluciones("geodude","graveler",2).
evoluciones("graveler","golem",3).
evoluciones("rhyhorn","rhydon",2).

%Debilidades

debilidades(piedra,planta,1.5).
debilidades(piedra,agua,1.5).

debilidades(agua,planta,2).
debilidades(agua,electricidad,1.5).

debilidades(electricidad,piedra,1.5).
debilidades(electricidad,fuego,1.5).

debilidades(fuego,agua,2).
debilidades(fuego,piedra,1.5).

debilidades(planta,fuego,2).
debilidades(planta,electricidad,1.5).

debilidades(_,_,1).


% definir cuanto baja cada habilidad

definirPoderesDeAtaque:-
  retractall(habilidad(_,_)),
  pokemonesEspeciales(PokemonesEspeciales),
  pokemones(PokemonesNormales),
  append(PokemonesNormales,PokemonesEspeciales,Pokemones),
  forall(
  (
    member(Pokemon,Pokemones),
    pokemonAtaques(Pokemon,Ataques)
  ),definirPoderDeAtaque(Ataques)).

definirPoderDeAtaque(Habilidades):-
  forall(member(Habilidad,Habilidades),
  (
    retractall(habilidad(Habilidad,_,_)),
    random(10,40,RandomMin),
    Max is 40 + (40 - RandomMin),
    assert(habilidad(Habilidad,RandomMin,Max))
  )).
  % cambiarHecho(habilidad(impactrueno,1000,1000),habilidad(impactrueno,_,_)).
pokemonesIniciales(["pikachu","bulbasaur","charmander","squirtle"]).

% CIUDADES
ciudades(["Paleta","Verde","Plateada","Celeste","Carmin","Azulona"]).

definirDistancias:-
  ciudades(Ciudades),
  definirDistancias(Ciudades).

definirDistancias(_,[]).
definirDistancias(Elemento1,[Elemento2|Cola]):-
  random(50,150,Random),
  cambiarHecho(distanciaEntreCiudades(Elemento1,Elemento2,_),distanciaEntreCiudades(Elemento1,Elemento2,Random)),
  definirDistancias(Elemento1,Cola).

definirDistancias([_|[]]).
definirDistancias([Elemento|Cola]):-
  definirDistancias(Elemento,Cola),
  definirDistancias(Cola).

% invierte origen y destino, duplicando virtualmente los hechos.
distancia(Ciudad1,Ciudad2,Distancia):-distanciaEntreCiudades(Ciudad1,Ciudad2,Distancia).
distancia(Ciudad1,Ciudad2,Distancia):-distanciaEntreCiudades(Ciudad2,Ciudad1,Distancia).

% genera los lideres con sus pokemones
generarLideresDeGimnasio:-
  ciudades(Ciudades),
  forall(nth0(Indice,Ciudades,Ciudad),generarLiderDeGimnasio(Ciudad,Indice)).

generarLiderDeGimnasio(Ciudad,Indice):-
  CantidadDePokemones is Indice + 1,
  pokemonesRandom(CantidadDePokemones,CantidadDePokemones,Pokemones),
  cambiarHecho(liderGimnasio(Ciudad,_),liderGimnasio(Ciudad,Pokemones)).
generarLiderDeGimnasio(_,_).

% dificultades de ciudad


% --- ESTADO ENTRENADOR


ciudadActual("Paleta").
pokemonesEntrenador([]).
pokebolasEntrenador([[normal,10,0.5],[normal,10,0.5],[normal,10,0.5]]).
dineroEntrenador(0).
pokehuevosEntrenador([]).
medallasEntrenador([]).


% --- ESTADO BILL


pokemonesBill([["charmander",fuego,120,120,excelente,0,1,["Lanzallamas","Onda ignea","Llamarada","Mordisco"],"pene"]]).
pokehuevosBill([[fuego,160],[agua,140],[piedra,120]]).





imprimirEstadoJugador:-
  ciudadActual(Ciudad),
  pokemonesEntrenador(Pokemones),
  pokebolasEntrenador(Pokebolas),
  dineroEntrenador(Dinero),
  pokehuevosEntrenador(Huevos),
  nl,
  write("Ciudad: "),write(Ciudad),nl,
  write("pokemones: "),write(Pokemones),nl,
  write("Pokebolas: "),write(Pokebolas),nl,
  write("Dinero: "),write(Dinero),nl,
  write("Huevos: "),write(Huevos),nl,
  nl.

init:-
  definirDistancias,
  definirPoderesDeAtaque,
  generarLideresDeGimnasio,
  shell(clear).

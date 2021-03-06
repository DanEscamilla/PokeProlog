% TODO
% mostrar barras de vida (con animaciones).
:-ensure_loaded("hechos.pl").
:-ensure_loaded("funciones-genericas.pl").
:-ensure_loaded("mapa.pl").




% JUEGO EMPIEZA AQUI
jugarPokemon:-
  init,
  elegirPokemonInicial,
  enCiudad.

enCiudad:-abrirMapaCiudad.

posibilidadesCiudad("Ir a hospital"):-
  irAHospital,
  enCiudad.
posibilidadesCiudad("Ir a gimnasio"):-
  irAGimnasio,
  enCiudad.
posibilidadesCiudad("Ir a tienda"):-
  irATienda,
  enCiudad.
posibilidadesCiudad("Inspeccionar Mochila"):-
  mostrarMochila,
  enCiudad.
posibilidadesCiudad("Ir a otra ciudad"):-
  irACiudad.

irACiudad:-
  abrirMapaGlobal.







%-------------------ESPECIFICAS POKEMON-------------------

retirarDeBill:-
  shell(clear),
  opcionesBill(Opciones),
  elegirOpcion("Que quieres retirar?",Opciones,Opcion),
  accionEligirRetiro(Opcion).

accionEligirRetiro("Salir").
accionEligirRetiro("Pokemon"):-
  elegirPokemonBill("Que pokemon quieres pedirle a Bill?",Pokemon),
  quitarPokemonBill(Pokemon),
  agregarPokemonNombradoAMochila(Pokemon).
accionEligirRetiro("Pokehuevo"):-
  shell(clear),
  pokehuevosBill(Pokehuevos),
  nl,elegirOpcion("Que pokehuevo quieres pedirle a Bill? ",Pokehuevos,Pokehuevo),nl,
  quitarPokehuevoBill(Pokehuevo),
  agregarPokehuevoAMochila(Pokehuevo).

opcionesBill(Opciones):-
  mostrarOpcionRetirarPokemon(["Salir"],L1),
  mostrarOpcionRetirarPokehuevo(L1,Opciones).

mostrarOpcionRetirarPokemon(L1,L2):- % esta opcion solo se muestra si hay mas de 1 un pokemon en mochila
  pokemonesBill([_|_]),
  append(L1,["Pokemon"],L2),nl.
mostrarOpcionRetirarPokemon(L1,L1). % esta opcion solo se muestra si hay mas de 1 un pokemon en mochila

mostrarOpcionRetirarPokehuevo(L1,L2):- % esta opcion solo se muestra si hay al menos 1 pokehuevo en mochila
  pokehuevosBill([_|_]),
  append(L1,["Pokehuevo"],L2),nl.
mostrarOpcionRetirarPokehuevo(L1,L1). % esta opcion solo se muestra si hay mas de 1 un pokemon en mochila

quitarPokemonBill(Pokemon):-
  pokemonesBill(Pokemones),
  nth0(_,Pokemones,Pokemon,PokemonesRestantes),
  cambiarHecho(pokemonesBill(_),pokemonesBill(PokemonesRestantes)).
quitarPokehuevoBill(Pokehuevo):-
  pokehuevosBill(Pokehuevos),
  nth0(_,Pokehuevos,Pokehuevo,PokehuevosRestantes),
  cambiarHecho(pokehuevosBill(_),pokehuevosBill(PokehuevosRestantes)).


% --- gimnasio

irAGimnasio:-
  imprimirLiderDeGimnasio,
  accionEntrarGimasio,
  esperarRespuesta1Enter,
  shell(clear).

imprimirLiderDeGimnasio:-
  ciudadActual(CiudadActual),
  liderGimnasio(CiudadActual,Pokemones),
  cambiarHecho(entrenadorActivo(_,_),entrenadorActivo(0,Pokemones)),
  shell(clear),
  nl,write("Entraste al gimnasio de: "),write(CiudadActual),nl,nl,
  write("Pokemones de lider de gimnasio: "),nl,
  mostrarPokemonesEnemigos(Pokemones),nl.

accionEntrarGimasio:-
  elegirOpcion("Deseas deseas retar al lider de gimnasio?",[salir,retar],OpcionElegida),
  accionEntrarGimasio(OpcionElegida).
accionEntrarGimasio:-accionEntrarGimasio.

accionEntrarGimasio(retar):-iniciarPeleaGimnasio.
accionEntrarGimasio(salir):-
  nl,write("Saliste del gimnasio"),nl,nl.



% --- pelea contra lider de gimnasio


iniciarPeleaGimnasio:-
  entrenadorActivo(_,Pokemones),
  cambiarHecho(peleandoCon(_),peleandoCon(gimnasio)),
  forall(member(Pokemon,Pokemones),iniciarPeleaGimnasio(Pokemon)),
  % gano el jugador si llega aqui
  obtenerMedalla.

iniciarPeleaGimnasio(Pokemon):-
  shell(clear),
  nl,write("El lider de gimnasio enemigo eligio su pokemon! "),nl,nl,
  mostrarPokemonEnemigo(Pokemon),nl,
  cambiarHecho(pokemonEnemigo(_),pokemonEnemigo(Pokemon)),
  esperarRespuesta1Enter,
  iniciarPelea.

obtenerMedalla:-
  ciudadActual(CiudadActual),
  medallasEntrenador(Medallas),
  not(member(CiudadActual,Medallas)),
  cambiarHecho(medallasEntrenador(_),medallasEntrenador([CiudadActual|Medallas])),
  write("Venciste al lider del gimnasio "),write(CiudadActual),write(", has ganado la medalla "),write(CiudadActual),nl,
  write("Tus medallas: "),write([CiudadActual|Medallas]),nl,nl,
  conseguisteTodasLasMedallas. % checa si ya venciste a todos los gimnasios
% ya tienes esta medalla
obtenerMedalla:-
  ciudadActual(CiudadActual),
  medallasEntrenador(Medallas),
  write("Venciste al lider del gimnasio "),write(CiudadActual),write(" de nuevo."),nl,
  write("Tus medallas: "),write(Medallas),nl,nl.


% --- Tienda


irATienda:-
  pokebolas(Pokebolas),
  dineroEntrenador(Dinero),
  shell(clear),
  nl,write("Que pokebola deseas comprar?"),nl,nl,
  write("0 - Regresar"),nl,
  mostrarPokebolasParaComprar(1),nl,
  write("Tienes "),write(Dinero),write(" PokePesos"),nl,
  elegirOpcion(["Regresar"|Pokebolas],Eleccion),!,
  comprarPokebola(Eleccion).


comprarPokebola("Regresar"):- shell(clear).
comprarPokebola(Pokebola):-
  Pokebola = [_,Precio|_],
  gastarDinero(Precio),
  agregarPokebolaAMochila(Pokebola),
  esperarRespuesta1Enter,
  irATienda.
comprarPokebola(_):-
  dineroEntrenador(Dinero),
  write("No tienes dinero suficiente para esa pokebola! solo tienes: "),write(Dinero),write(" PokePesos"),nl,nl,
  esperarRespuesta1Enter,
  irATienda.


% --- llegar a ciudad


% llegarACiudad:-
%   ciudadDestino(Destino),
%   cambiarHecho(ciudadActual(_),ciudadActual(Destino)),
%   write("LLegaste a "),write(Destino),write("."),nl,nl.


% --- eclosionar huevos


actualizarPokehuevo:-
  pokehuevosEntrenador(Pokehuevos),
  forall(member(Pokehuevo,Pokehuevos),(
    sumarDistanciaAPokehuevo(Pokehuevo,1)
  )).

sumarDistanciaAPokehuevo([Nombre,DistanciaNecesaria],Distancia):-
  DistanciaRecorridaActualizada is DistanciaNecesaria - Distancia,
  DistanciaRecorridaActualizada > 0,
  PokehuevoActualizado = [Nombre,DistanciaRecorridaActualizada],
  actualizarPokehuevo([Nombre,DistanciaNecesaria],PokehuevoActualizado).
  % write("Huevo tipo "),write(Nombre),write(" necesita "),write(DistanciaRecorridaActualizada),write("km para eclosionar."),nl,nl.

% Caso en que ya esta listo para eclosionar
sumarDistanciaAPokehuevo(Pokehuevo,_):-
  Pokehuevo = [Tipo,_],
  pokemonesDeTipo(Tipo,PokemonesDeTipoCompatible),
  random_permutation(PokemonesDeTipoCompatible,[PokemonEclosionado|_]),
  sacarPokehuevoDeMochila(Pokehuevo),
  obtenerNombre(PokemonEclosionado,NombrePokemon),
  shell(clear),
  nl,write("Un pokehuevo de "),write(Tipo),write(" eclosiono a un "),write(NombrePokemon),nl,
  agregarPokemonAMochila(PokemonEclosionado),
  esperarRespuesta1Enter.

% Econtrar pokemones de un tipo
pokemonesDeTipo(Tipo,PokemonesDeTipo):-
  pokemones(Pokemones),
  findall(PokemonDeTipo,(
    member(PokemonDeTipo,Pokemones),
    PokemonDeTipo = [_,Tipo|_]%el tipo del pokemon es igual a
  ),PokemonesDeTipo).


% -- Hospital


irAHospital:-
  shell(clear),
  pokemonesEntrenador(Pokemones),
  findall(PokemonRestaurado,
  (
    member(Pokemon,Pokemones),
    restaurarVida(Pokemon,PokemonRestaurado)
  ),PokemonesRestaurados),
  cambiarHecho(pokemonesEntrenador(_),pokemonesEntrenador(PokemonesRestaurados)),
  nl,write("Tus pokemones han sido restaurados!"),nl,nl,
  mostrarPokemonesEntrenador,nl,
  esperarRespuesta1Enter,
  shell(clear).


% --- encontrar entrenador


encontrarEntrenador:-
  transicionSalida,
  generarEntrenador,
  imprimirEntrenadorActivo,
  accionEncontrarEntrenador.

generarEntrenador:-
  dineroRandom(Dinero),
  pokemonesRandom(2,Pokemones),
  cambiarHecho(entrenadorActivo(_,_),entrenadorActivo(Dinero,Pokemones)).

imprimirEntrenadorActivo:-
  entrenadorActivo(Dinero,Pokemones),
  nl,write("Encontraste un entrenador:"),nl,
  write("Dinero: "),write(Dinero),nl,
  write("Pokemones: "),nl,
  mostrarPokemonesEnemigos(Pokemones),nl.

dineroRandom(Dinero):-random(60,120,Dinero).

accionEncontrarEntrenador:-
  elegirOpcion("Deseas aceptar el duelo o rechazarlo?",[rechazar,aceptar],OpcionElegida),
  accionEncontrarEntrenador(OpcionElegida).
accionEncontrarEntrenador:-accionEncontrarEntrenador.

accionEncontrarEntrenador(aceptar):-
  iniciarPeleaEntrenador,
  esperarRespuesta1Enter.
accionEncontrarEntrenador(rechazar).


% --- pelea contra entrenador


iniciarPeleaEntrenador:-
  entrenadorActivo(Dinero,Pokemones),
  cambiarHecho(peleandoCon(_),peleandoCon(entrenador)),
  forall(member(Pokemon,Pokemones),iniciarPeleaEntrenador(Pokemon)),
  % gano el jugador si llega aqui
  obtenerDinero(Dinero).

iniciarPeleaEntrenador(Pokemon):-
  shell(clear),
  nl,write("El entrenador enemigo eligio su pokemon! "),nl,
  mostrarPokemonEnemigo(Pokemon),nl,
  cambiarHecho(pokemonEnemigo(_),pokemonEnemigo(Pokemon)),
  esperarRespuesta1Enter,
  iniciarPelea.

obtenerDinero(Dinero):-
  dineroEntrenador(DineroActual),
  Mitad is Dinero/2,
  NuevoDinero is Mitad + DineroActual,
  write("Ganaste "),write(Mitad),write(" PokePesos por ganar la batalla"),nl,nl,
  cambiarHecho(dineroEntrenador(_),dineroEntrenador(NuevoDinero)).


% --- encontrar pokemon


encontrarPokemon:-
  cambiarHecho(peleandoCon(_),peleandoCon(pokemonSalvaje)),
  pokemonRandom(Pokemon),
  encontrarPokemon(Pokemon).
encontrarPokemon(Pokemon):-
  transicionSalida,
  Pokemon = [Nombre|_],
  nl,write("Encontraste un "),write(Nombre),write(" salvaje!"),nl,
  mostrarPokemonEnemigo(Pokemon),nl,
  accionEncontrarPokemon(Pokemon).

pokemonRandom(Pokemon):-
  random(Rand),
  Rand < 0.99,
  pokemones(Pokemones),
  elementoRandomLista(Pokemones,Pokemon).
pokemonRandom(Pokemon):-
  pokemonesEspeciales(Pokemones),
  elementoRandomLista(Pokemones,Pokemon).

accionEncontrarPokemon(Pokemon):-
  elegirOpcion("Deseas pelear o correr?",[correr,pelear],OpcionElegida),
  accionEncontrarPokemon(OpcionElegida,Pokemon).
accionEncontrarPokemon(Pokemon):-accionEncontrarPokemon(Pokemon).

accionEncontrarPokemon(pelear,Pokemon):-
  cambiarHecho(pokemonEnemigo(_),pokemonEnemigo(Pokemon)),
  iniciarPelea,
  esperarRespuesta1Enter.
accionEncontrarPokemon(correr,_).

% --- iniciar pelea ---


iniciarPelea:-
  tienesPokemonesVivos,
  elegirPokemonParaPelea,
  pelea.
iniciarPelea:-perdisteElJuego.

elegirPokemonParaPelea:-
  shell(clear),
  elegirTuPokemon("Que pokemon vas a utilizar para la pelea?",Pokemon),
  establezerPokemonParaPelea(Pokemon).

establezerPokemonParaPelea(Pokemon):-
  pokemonEstado(Pokemon,Estado),
  Estado \= caido,
  cambiarHecho(pokemonActivo(_),pokemonActivo(Pokemon)).

establezerPokemonParaPelea(Pokemon):-
  nombreMiPokemon(Pokemon,Nombre),
  write(Nombre),write(" esta caido, para poder utilizarlo debes revivirlo en el hospital pokemon."),nl,nl,
  esperarRespuesta1Enter,
  elegirPokemonParaPelea.

tienesPokemonesVivos:-
  pokemonesEntrenador(Pokemones),
  member(Pokemon,Pokemones),
  estaVivo(Pokemon).

% --- pelea ---

pelea:-ataqueJugador.

ataqueJugador:-
  mostrarBarrasDeVida,
  pokemonActivo(Pokemon),
  pokemonEnemigo(PokemonEnemigo),
  estaVivo(Pokemon),
  elegirAtaque(Ataque),!,
  calcularPoderAtaque(Pokemon,Ataque,PoderAtaque),
  calcularEfectividad(Pokemon,PokemonEnemigo,PoderAtaque,PoderAtaqueModificado,Modificador),
  atacarPokemon(PoderAtaqueModificado,PokemonEnemigo,PokemonEnemigoLastimado),
  cambiarHecho(pokemonEnemigo(_),pokemonEnemigo(PokemonEnemigoLastimado)),
  nombreMiPokemon(Pokemon,Nombre),
  obtenerNombre(PokemonEnemigo,NombreEnemigo),
  pokemonVidaActual(PokemonEnemigoLastimado,VidaRestanteEnemigo),
  mostrarBarrasDeVida,
  write(Nombre),write(" utilizo "),write(Ataque),mensajeEfectividad(Modificador),write(" hiciste "),write(PoderAtaqueModificado),write(" de dano!"),nl,
  write("El "),write(NombreEnemigo),write(" enemigo tiene "),write(VidaRestanteEnemigo),write(" de vida restante."),nl,nl,
  esperarRespuesta1Enter,
  ataqueEnemigo.
ataqueJugador:-
  terminoPelea.

ataqueEnemigo:-
  pokemonActivo(Pokemon),
  pokemonEnemigo(PokemonEnemigo),
  estaVivo(PokemonEnemigo),
  elegirAtaqueRandom(PokemonEnemigo,Ataque),
  calcularPoderAtaque(PokemonEnemigo,Ataque,PoderAtaque),
  calcularEfectividad(PokemonEnemigo,Pokemon,PoderAtaque,PoderAtaqueModificado,Modificador),
  atacarPokemon(PoderAtaqueModificado,Pokemon,PokemonLastimado),
  cambiarHecho(pokemonActivo(_),pokemonActivo(PokemonLastimado)),
  nombreMiPokemon(Pokemon,Nombre),
  obtenerNombre(PokemonEnemigo,NombreEnemigo),
  pokemonVidaActual(PokemonLastimado,VidaRestante),
  mostrarBarrasDeVida,
  nl,write("El "),write(NombreEnemigo),write(" enemigo utilizo "),write(Ataque),mensajeEfectividad(Modificador),write(" te hizo "),write(PoderAtaqueModificado),write(" de dano!"),nl,
  write(Nombre),write(" tiene "),write(VidaRestante),write(" de vida restante."),nl,nl,
  esperarRespuesta1Enter,
  ataqueJugador.
ataqueEnemigo:-
  terminoPelea.

% Elige un ataque para el enemigo
elegirAtaqueRandom(Pokemon,Ataque):-
  pokemonAtaques(Pokemon,Ataques),
  random_permutation(Ataques,[Ataque|_]).

% Pide a jugador que eliga un ataque.
elegirAtaque(Ataque):-
  pokemonActivo(Pokemon),
  pokemonAtaques(Pokemon,Ataques),
  nl,write("Elige tu ataque:"),nl,nl,
  mostrarAtaques(Pokemon),
  elegirOpcion(Ataques,Ataque).
elegirAtaque(Ataque):-elegirAtaque(Ataque).

atacarPokemon(PoderAtaque,PokemonAtacado,PokemonLastimado):-
  recibirDano(PokemonAtacado,PoderAtaque,PokemonLastimado).

recibirDano(Pokemon,PoderAtaque,PokemonLastimado):-
  Pokemon = [Nombre,Tipo,VidaMax,VidaActual,_,Experiencia,Nivel,Ataques|C],
  NuevaVida is VidaActual - PoderAtaque,
  calcularEstado(NuevaVida,VidaMax,NuevoEstado),
  PokemonLastimado = [Nombre,Tipo,VidaMax,NuevaVida,NuevoEstado,Experiencia,Nivel,Ataques|C].

calcularEstado(VidaActual,_,caido):-
  VidaActual =< 0.
calcularEstado(VidaActual,VidaMaxima,critico):-
  VidaActual < (VidaMaxima * 0.25).
calcularEstado(_,_,excelente).

calcularPoderAtaque(Pokemon,Ataque,PoderAtaque):-
  pokemonNivel(Pokemon,Nivel),
  habilidad(Ataque,PoderMin,PoderMax),
  PoderMinAjustado is PoderMin + ((PoderMin * Nivel)/5),
  PoderMaxAjustado is PoderMax + ((PoderMax * Nivel)/5),
  random(PoderMinAjustado,PoderMaxAjustado,Rand),
  PoderAtaque is round(Rand) .

calcularEfectividad(Pokemon,PokemonEnemigo,PoderAtaque,PoderModificado,Modificador):-
  pokemonTipo(Pokemon,Tipo),
  pokemonTipo(PokemonEnemigo,TipoEnemigo),
  debilidades(TipoEnemigo,Tipo,Modificador),
  PoderModificado is PoderAtaque * Modificador.
calcularEfectividad(_,_,PoderAtaque,PoderAtaque,1).

mensajeEfectividad(2):-write(", es Super efectivo!").
mensajeEfectividad(1.5):-write(", es Efectivo!").
mensajeEfectividad(_):-write(",").

% --- terminar pelea ---


terminoPelea:-
  shell(clear),nl,
  tab(5),write("Termino la pelea"),nl,nl,
  validarGanador,
  esperarRespuesta1Enter,
  shell(clear).

validarGanador:-
  pokemonActivo(Pokemon),
  estaVivo(Pokemon),
  imprimirGanarPelea(Pokemon),
  aumentarExperiencia(Pokemon,40,NuevoPokemon), % incrementa experiencia en 40.
  evolucionar(NuevoPokemon,PokemonEvolucionado), % si el pokemon no evoluciona, Pokemon y PokemonEvolucionado son el mismo.
  actualizarPokemonActivo(PokemonEvolucionado),
  capturarPokemon.

validarGanador:-
  pokemonActivo(Pokemon),
  actualizarPokemonActivo(Pokemon),
  imprimirPerderPelea(Pokemon),
  iniciarPelea.

imprimirGanarPelea(Pokemon):-
  nombreMiPokemon(Pokemon,Nombre),
  write(Nombre),write(" gano la pelea!"),nl,nl.
imprimirPerderPelea(Pokemon):-
  nombreMiPokemon(Pokemon,Nombre),
  write(Nombre),write(" perdio la pelea, necesita atencion medica."),nl,nl.


actualizarPokemonActivo(Pokemon):-
  nombreMiPokemon(Pokemon,Nombre),
  pokemonesEntrenador(Pokemones),
  remplazarElemento([_,_,_,_,_,_,_,_,Nombre],Pokemon,Pokemones,NuevosPokemones),
  cambiarHecho(pokemonesEntrenador(_),pokemonesEntrenador(NuevosPokemones)).

% No subio de nivel
aumentarExperiencia(Pokemon,Incremento,NuevoPokemon):-
  nombreMiPokemon(Pokemon,NombreDado),
  pokemonNivel(Pokemon,Nivel),
  pokemonExperiencia(Pokemon,Experiencia),
  NuevaExperiencia is Experiencia + Incremento,
  calculaExperienciaNecesaria(Nivel,NuevaExperiencia,ExperienciaNecesaria),
  ExperienciaNecesaria > 0,
  actualizaExperiencia(Pokemon,NuevaExperiencia,NuevoPokemon),
  write(NombreDado),write(" obtuvo "),write(Incremento),write(" de experiencia."),nl,write("Experiencia faltante para subir nivel: "),write(ExperienciaNecesaria),nl,nl.

% Subio de nivel
aumentarExperiencia(Pokemon,Incremento,NuevoPokemon):-
  Pokemon = [Nombre,Tipo,VidaMax,_,_,Experiencia,Nivel,Ataques,NombrePersonalizado],
  ExperienciaIncrementada is Experiencia + Incremento,
  calculaExperienciaNecesaria(Nivel,ExperienciaIncrementada,ExperienciaNecesaria),
  NuevaExperiencia is -ExperienciaNecesaria, % Subio de nivel, experiencia actual es la experiencia que sobra postiva
  NuevoNivel is Nivel + 1,
  NuevaVidaMax is VidaMax + 20,
  NuevoPokemon = [Nombre,Tipo,NuevaVidaMax,NuevaVidaMax,excelente,NuevaExperiencia,NuevoNivel,Ataques,NombrePersonalizado],
  write(NombrePersonalizado),write(" ha subido de nivel!!! Ahora es nivel "),write(NuevoNivel),nl,nl.

%  Calular experiencia necesaria para subir de nivel
calculaExperienciaNecesaria(Nivel,ExperienciaActual,ExperienciaNecesaria):-
  ExperienciaNecesaria is 100 + (Nivel * 30) - ExperienciaActual.

% Evoluciono
evolucionar(Pokemon,PokemonEvolucionado):-
  evolucionarSinMensaje(Pokemon,PokemonEvolucionado),
  PokemonEvolucionado\=Pokemon,
  obtenerNombre(Pokemon,Nombre),
  obtenerNombre(PokemonEvolucionado,NombreEvolucion),
  write("Tu "),write(Nombre),write(" evoluciono a un "),write(NombreEvolucion),write("!!"),nl,nl.
% No evoluciono
evolucionar(Pokemon,Pokemon).

evolucionarSinMensaje(Pokemon,PokemonEvolucionado):-
  pokemonNivel(Pokemon,Nivel),
  obtenerNombre(Pokemon,Nombre),
  evoluciones(Nombre,NombreEvolucion,NivelNecesario),
  Nivel>=NivelNecesario,
  actualizarNombre(Pokemon,NombreEvolucion,PokemonIntermedio),
  evolucionarSinMensaje(PokemonIntermedio,PokemonEvolucionado).
evolucionarSinMensaje(Pokemon,Pokemon).


% --- capturar pokemon ---


capturarPokemon:- not(peleandoCon(pokemonSalvaje)). % no se puede capturar si no esta peleando co pokemonSalvaje
capturarPokemon:- % no se puede capturar si el entrenado no tiene pokebolas
  pokebolasEntrenador([]),
  write("Ya no tienes pokebolas, no puedes capturar al pokemon, consigue mas!"),nl.

capturarPokemon:-
  pokemonEnemigo(PokemonVencido),
  obtenerNombre(PokemonVencido,TipoPokemon),
  shell(clear),
  nl,write("Deseas Capturar al "),write(TipoPokemon),write("?"),nl,
  mostrarPokemonEnemigo(PokemonVencido),
  elegirOpcion("",[no,si],Opcion),nl,
  accionCapturarPokemon(Opcion,PokemonVencido).

accionCapturarPokemon(si,Pokemon):-
  pokebolasEntrenador(PokebolasEntrenador),
  shell(clear),
  nl,elegirOpcion("Que pokebola deseas utilizar?",PokebolasEntrenador,PokebolaElegida),nl,
  intentarCapturar(PokebolaElegida,Pokemon).

accionCapturarPokemon(no,_).

intentarCapturar(Pokebola,Pokemon):-
  Pokebola = [_,_,Probabilidad],
  random(Random),
  Random < Probabilidad,
  sacarPokebolaMochila(Pokebola),
  write("Lo capturaste!!"),nl,nl,
  agregarPokemonAMochila(Pokemon).
intentarCapturar(Pokebola,[NombrePokemon|_]):-
  sacarPokebolaMochila(Pokebola),
  write("El "),write(NombrePokemon),write(" escapo! mejor suerte la proxima"),nl,nl.

sacarPokebolaMochila(Pokebola):-
  pokebolasEntrenador(Pokebolas),
  nth0(_,Pokebolas,Pokebola,PokebolasRestantes),!,
  cambiarHecho(pokebolasEntrenador(_),pokebolasEntrenador(PokebolasRestantes)).

% --- Encuentra pokehuevo random y agrega a mochila

encontrarPokehuevo:-
  shell(clear),
  pokehuevoRandom(Pokehuevo),
  Pokehuevo = [Nombre|_],
  nl,write("Encontraste un nuevo pokehuevo de "),write(Nombre),write("!"),nl,nl,
  agregarPokehuevoAMochila(Pokehuevo).

pokehuevoRandom(Pokehuevo):-
  pokehuevos(Pokehuevos),
  elementoRandomLista(Pokehuevos,Pokehuevo).


% --- Encuentra pokebola random y agrega a mochila

encontrarPokebola:-
  shell(clear),
  pokerbolaRandom(Pokebola),
  Pokebola = [Nombre|_],
  nl,write("Encontraste una pokebola "),write(Nombre),write("!"),nl,nl,
  agregarPokebolaAMochila(Pokebola),
  esperarRespuesta1Enter.

pokerbolaRandom(Pokebola):-
  pokebolas(Pokebolas),
  elementoRandomLista(Pokebolas,Pokebola).

% --- Elegir ciudad para ir y actualiza hecho

% elegirCiudadParaIr:-
%   elegirCiudadParaIr(CiudadDestino),
%   accionElegirCiudadParaIr(CiudadDestino).
%
% accionElegirCiudadParaIr("Regresar"):-enCiudad.
% accionElegirCiudadParaIr(CiudadDestino):-
%   cambiarHecho(ciudadDestino(_),ciudadDestino(CiudadDestino)),
%   nl,write("Te diriges a "),write(CiudadDestino),nl,
%   llendoACiudad.

% --- eliges entre los pokemones iniciales validos y se agrega a tu mochila.

elegirPokemonInicial:-
  pokemonesIniciales(PokemonesIniciales),
  nl,elegirOpcion("Elige tu pokemon inicial: ",PokemonesIniciales,NombrePokemon),
  obtenerPokemonEnBaseANombre(NombrePokemon,Pokemon),
  agregarPokemonAMochila(Pokemon),
  esperarRespuesta1Enter,
  shell(clear).

% PERDISTE

perdisteElJuego:-
  write("Te quedaste sin pokemones para pelear!"),nl,
  write("Mejor suerte la proxima"),nl,nl,
  abort.

% Capturaste a todos

checarSiCapturasteATodos:-
  checarSiCapturasteATodos2.
checarSiCapturasteATodos.

checarSiCapturasteATodos2:-
  pokemonesEntrenador(PokemonesEntrenador),
  pokemonesBill(PokemonesBill),
  append(PokemonesEntrenador,PokemonesBill,TodosTusPokemones),
  listaDeTipos(TiposExisten),
  listaDeTipos([],MisTipos,TodosTusPokemones),!,
  length(TiposExisten,LongitudTipos),
  length(MisTipos,LongitudTipos),
  write("Has capturado a todos los pokemones!!"),nl,
  write("Felicidades, Ganaste!"),nl,nl,
  abort.


listaDeTipos(Lista):-
  pokemones(Pokemones),
  listaDeTipos([],Lista,Pokemones).
listaDeTipos(Lista,Lista,[]).
listaDeTipos(Acumulador,Lista,[Pokemon|Pokemones]):-
  Pokemon = [_,Tipo|_],
  not(member(Tipo,Acumulador)),
  listaDeTipos([Tipo|Acumulador],Lista,Pokemones).
listaDeTipos(Acumulador,Lista,[_|Pokemones]):-listaDeTipos(Acumulador,Lista,Pokemones).


obtenerNombresPokemones(Pokemones,PokemonesNombre):-
  findall(Nombre,
  (
    member(Pokemon,Pokemones),
    obtenerNombre(Pokemon,Nombre)
  ),PokemonesNombre).

% Conseguiste todas las medallas

conseguisteTodasLasMedallas:-
  findall(Gimnasio,liderGimnasio(Gimnasio,_),Gimnasios),
  medallasEntrenador(Medallas),
  sort(Medallas,MedallasOrdenadas),
  sort(Gimnasios,GimnasiosOrdenados),
  MedallasOrdenadas = GimnasiosOrdenados,
  write("Conesguiste todas las medallas!!"),nl,
  write("Felicidades, Ganaste!"),nl,nl,
  abort.
conseguisteTodasLasMedallas.







%-------------------GENERALES POKEMON-------------------








% --- gastar dinero


gastarDinero(Cantidad):-
  dineroEntrenador(Dinero),
  Dinero>=Cantidad,
  NuevoDinero is Dinero - Cantidad,
  cambiarHecho(dineroEntrenador(_),dineroEntrenador(NuevoDinero)),
  write("Has gastado "),write(Cantidad),write(" PokePesos, te quedan "),write(NuevoDinero),nl,nl.


% --- restaurar vida

restaurarVida(Pokemon,PokemonRestaurado):-
  pokemonVidaMax(Pokemon,VidaMax),
  remplazarIndice(3,VidaMax,Pokemon,PokemonConVida),
  remplazarIndice(4,excelente,PokemonConVida,PokemonRestaurado).

% --- pokemon esta vivo?

estaVivo(Pokemon):-
  pokemonVidaActual(Pokemon,VidaActual),
  VidaActual>0.

% --- mostrar opciones de pokebolas

mostrarPokebolasParaComprar(IndiceInicial):-
  pokebolas(Pokebolas),
  forall(nth0(Indice,Pokebolas,Pokebola),
  (
    Pokebola = [Nombre,Precio,_],
    IndiceAjustado is Indice + IndiceInicial,
    write(IndiceAjustado),write(" - "),write(Nombre),write(" cuesta "),write(Precio),write(" PokePesos"),nl
  )).

% --- elegirCiudadParaIr
elegirCiudadParaIr(Ciudad):-
  ciudades(Ciudades),
  ciudadActual(CiudadActual),
  delete(Ciudades,CiudadActual,Destinos),
  write("A que ciudad vas a viajar?"),nl,
  write("0 - Regresar"),nl,
  mostrarCiudades(1,Destinos),
  elegirOpcion(["Regresar"|Destinos],Ciudad).
% --- mostrar opciones de pokebolas

mostrarCiudades(Destinos):-mostrarCiudades(0,Destinos).
mostrarCiudades(IndiceInicial,Destinos):-
  ciudadActual(CiudadActual),
  forall(nth0(Indice,Destinos,Ciudad),(
    distancia(CiudadActual,Ciudad,Distancia),
    IndiceAjustado is Indice + IndiceInicial,
    write(IndiceAjustado),write(" - "),write(Ciudad),write(" esta a  "),write(Distancia),write(" kilometros"),nl
  )).


% --- Similar a elegirOpcion, pero muestra el nombre del pokemon antes de la lista

elegirTuPokemon(Mensaje,Pokemon):-
  pokemonesEntrenador(Pokemones),
  elegirPokemon(Mensaje,Pokemones,Pokemon).
elegirPokemonBill(Mensaje,Pokemon):-
  pokemonesBill(Pokemones),
  elegirPokemon(Mensaje,Pokemones,Pokemon).

elegirPokemon(Mensaje,Pokemones,Pokemon):-
  shell(clear),
  nl,write(Mensaje),nl,nl,
  mostrarPokemones(Pokemones),
  leerOpcion(Eleccion),nl,
  nth0(Eleccion,Pokemones,Pokemon).
elegirPokemon(Mensaje,Pokemones,Pokemon):-
  write("Opcion invalida"),nl,nl,
  esperarRespuesta1Enter,
  elegirPokemon(Mensaje,Pokemones,Pokemon).

mostrarPokemonesEntrenador:-
  pokemonesEntrenador(Pokemones),
  mostrarPokemones(Pokemones).


% -- Imprime Lista de pokemones del jugador enumerada por renglon y con el nombre que se le dio al pokemon

mostrarPokemones(Pokemones):-
  forall(nth0(Indice,Pokemones,Pokemon),
  (
    Pokemon = [Nombre,Tipo,VidaMax,VidaActual,Estado,Experiencia,Nivel,Ataques,NombreDado],
    calculaExperienciaNecesaria(Nivel,Experiencia,ExperienciaNecesaria),
    write(Indice),write(" - "),write(NombreDado),write(":"),nl,
    tab(5),write("Pokemon: "),write(Nombre),write(", Tipo: "), write(Tipo),nl,
    tab(5),write("Vida Actual: "),write(VidaActual), write(", Vida Maxima: "),write(VidaMax),write(", Estado: "),write(Estado),nl,
    tab(5),write("Nivel: "),write(Nivel),write(", Experiencia para siguiente nivel: "),write(ExperienciaNecesaria),nl,
    tab(5),write("Ataques: "),write(Ataques),nl
  )).

% -- Imprime Lista de pokemones del jugador enumerada por renglon y con el nombre que se le dio al pokemon

mostrarPokemonesEnemigos(Pokemones):-
  forall(nth0(_,Pokemones,Pokemon),
  (
    mostrarPokemonEnemigo(Pokemon)
  )).
mostrarPokemonEnemigo(Pokemon):-
  Pokemon = [Nombre,Tipo,VidaMax,_,_,_,Nivel,Ataques],
  tab(2),write(Nombre),nl,
  tab(5),write("Tipo: "), write(Tipo),write(", Vida: "),write(VidaMax),write(", Nivel: "),write(Nivel),nl,
  tab(5),write("Ataques: "),write(Ataques),nl.

% --- mostrar ataques

mostrarAtaques(Pokemon):-
  pokemonAtaques(Pokemon,Ataques),
  forall(nth0(Indice,Ataques,Ataque),
  (
    write(Indice),write(" - "),
    mostrarAtaque(Ataque)
  )).
mostrarAtaque(Ataque):-
  habilidad(Ataque,Min,Max),
  write(Ataque),
  write(", poder min: "),write(Min),
  write(", poder max: "),write(Max),nl.

% --- agregar pokemon a mochila

agregarPokemonAMochila(Pokemon):-
  hayEspacioEnMochila,
  nombrarPokemon(Pokemon,PokemonNombrado),
  obtenerNombre(Pokemon,Nombre),
  nl,write("Tienes un nuevo "),write(Nombre),write("!!"),nl,nl,
  pokemonesEntrenador(Pokemones),
  append(Pokemones,[PokemonNombrado],NuevosPokemones),
  cambiarHecho(pokemonesEntrenador(_),pokemonesEntrenador(NuevosPokemones)),
  checarSiCapturasteATodos.
agregarPokemonAMochila(Pokehuevo):-accionNoEspacioParaPokemon(Pokehuevo).

agregarPokemonNombradoAMochila(PokemonNombrado):-
  hayEspacioEnMochila,
  pokemonesEntrenador(Pokemones),
  append(Pokemones,[PokemonNombrado],NuevosPokemones),
  cambiarHecho(pokemonesEntrenador(_),pokemonesEntrenador(NuevosPokemones)),
  nombreMiPokemon(PokemonNombrado,Nombre),
  write(Nombre),write(" fue agregado a tu mochila."),nl,nl,
  esperarRespuesta1Enter.
agregarPokemonNombradoAMochila(PokemonNombrado):-agregarPokemonNombradoAMochila(PokemonNombrado).

% Mochila llena, decidir accion para pokehuevo
accionNoEspacioParaPokemon(Pokehuevo):-
  mostrarOpcionesMochilaLLena("No hay espacio en mochila, Que deseas hacer con el pokemon?",Opciones),
  leerOpcion(Opciones,Eleccion),nl,
  decisionPokemon(Eleccion,Pokehuevo).
accionNoEspacioParaPokemon(Pokehuevo):-accionNoEspacioParaPokemon(Pokehuevo).

% posibles acciones
decisionPokemon(0,_). %dejarlo
decisionPokemon(1,Pokemon):- %deshacerte de un pokemon
  mandarPokemonABill,
  agregarPokemonAMochila(Pokemon). %deshacerte de un pokehuevo
decisionPokemon(2,Pokemon):-
  mandarPokehuevoABill,
  agregarPokemonAMochila(Pokemon).

% -- Agregar pokehuevo a mochila

agregarPokehuevoAMochila(Pokehuevo):-
  hayEspacioEnMochila,
  pokehuevosEntrenador(Pokehuevos),
  obtenerNombre(Pokehuevo,Nombre),
  write("Tienes un nuevo pokehuevo de tipo "),write(Nombre),write("!"),nl,nl,
  cambiarHecho(pokehuevosEntrenador(_),pokehuevosEntrenador([Pokehuevo|Pokehuevos])),
  esperarRespuesta1Enter.
agregarPokehuevoAMochila(Pokehuevo):-accionNoEspacioParaPokehuevo(Pokehuevo).

% --- Mochila llena, decidir accion para pokehuevo

accionNoEspacioParaPokehuevo(Pokehuevo):-
  mostrarOpcionesMochilaLLena("No hay espacio en mochila, Que deseas hacer con el pokehuevo?",Opciones),
  leerOpcionesMochilaLLena(Opciones,Eleccion),nl,
  decisionPokehuevo(Eleccion,Pokehuevo).
accionNoEspacioParaPokehuevo(Pokehuevo):-accionNoEspacioParaPokehuevo(Pokehuevo).

leerOpcionesMochilaLLena(Opciones,Eleccion):-
  leerOpcion(Eleccion),nl,
  member(Eleccion,Opciones).
leerOpcionesMochilaLLena(Opciones,Eleccion):-leerOpcionesMochilaLLena(Opciones,Eleccion).


% posibles acciones
decisionPokehuevo(0,_). %dejarlo
decisionPokehuevo(1,Pokehuevo):- %deshacerte de un pokemon
  mandarPokemonABill,
  agregarPokehuevoAMochila(Pokehuevo). %deshacerte de un pokehuevo
decisionPokehuevo(2,Pokehuevo):-
  mandarPokehuevoABill,
  agregarPokehuevoAMochila(Pokehuevo).

% -- Muestra opciones dependiendo del estado de la mochila

mostrarOpcionesMochilaLLena(Mensaje,Opciones):-
  write(Mensaje),nl,
  write("0-dejarlo"),nl,
  mostrarOpcionMandarPokemonABill([0],L1),
  mostrarOpcionMandarPokehuevoABill(L1,Opciones).

mostrarOpcionMandarPokemonABill(L1,L2):- % esta opcion solo se muestra si hay mas de 1 un pokemon en mochila
  pokemonesEntrenador([_,_|_]),
  write("1-Mandar pokemon a bill"),
  append(L1,[1],L2),nl.
mostrarOpcionMandarPokemonABill(L1,L1). % esta opcion solo se muestra si hay mas de 1 un pokemon en mochila

mostrarOpcionMandarPokehuevoABill(L1,L2):- % esta opcion solo se muestra si hay al menos 1 pokehuevo en mochila
  pokehuevosEntrenador([_|_]),
  write("2-Mandar pokehuevo a bill"),
  append(L1,[2],L2),nl.
mostrarOpcionMandarPokehuevoABill(L1,L1). % esta opcion solo se muestra si hay mas de 1 un pokemon en mochila


% -- Verifica si hay menos de 6 pokemones/huevos en la mochila

hayEspacioEnMochila:-
  pokemonesEntrenador(Pokemones),
  pokehuevosEntrenador(Pokehuevos),
  length(Pokemones,CantidadPokemones),
  length(Pokehuevos,CantidadPokehuevos),
  Suma is CantidadPokemones + CantidadPokehuevos,
  Suma < 6.

% --- mandar pokemon a bill

mandarPokemonABill:-
  elegirTuPokemon("Que pokemon vas a mandar a bill?",Pokemon),
  mandarPokemonABill(Pokemon).

mandarPokemonABill(Pokemon):-
  pokemonesEntrenador(Pokemones),
  nth0(_,Pokemones,Pokemon,PokemonesRestantes),
  pokemonesBill(PokemonesBill),
  cambiarHecho(pokemonesBill(_),pokemonesBill([Pokemon|PokemonesBill])),
  cambiarHecho(pokemonesEntrenador(_),pokemonesEntrenador(PokemonesRestantes)),
  nombreMiPokemon(Pokemon,NombreDado),
  write("Mandaste a "),write(NombreDado),write(" con bill!"),nl.

% --- mandar pokehuevo a bill


mandarPokehuevoABill:-
  pokehuevosEntrenador(Pokehuevos),
  elegirOpcion("Que pokehuevo vas a mandar a Bill? ",Pokehuevos,Pokehuevo),
  mandarPokehuevoABill(Pokehuevo).

mandarPokehuevoABill(Pokehuevo):-
  pokehuevosEntrenador(Pokehuevos),
  nth0(_,Pokehuevos,Pokehuevo,PokehuevosRestantes),
  pokehuevosBill(PokehuevosBill),
  cambiarHecho(pokehuevosBill(_),pokehuevosBill([Pokehuevo|PokehuevosBill])),
  cambiarHecho(pokehuevosEntrenador(_),pokehuevosEntrenador(PokehuevosRestantes)),
  obtenerNombre(Pokehuevo,Nombre),
  write("Mandaste un pokehuevo tipo "),write(Nombre),write(" con bill!"),nl,nl.


% -- Agregar pokebola a mochila

agregarPokebolaAMochila(Pokebola):-
  pokebolasEntrenador(Pokebolas),
  obtenerNombre(Pokebola,Nombre),
  cambiarHecho(pokebolasEntrenador(_),pokebolasEntrenador([Pokebola|Pokebolas])),
  write("Tienes una nueva pokebola "),write(Nombre),nl,nl.


% -- buscar pokemon utilizando el nombre

obtenerPokemonEnBaseANombre(Nombre,Pokemon):-
  pokemones(Pokemones),
  Pokemon = [Nombre|_],
  member(Pokemon,Pokemones).


% --- Le das un pokemon y regresa un pokemon nombrado (1 attributo extra al final que representa el nombre dado)

nombrarPokemon(Pokemon,PokemonNombrado):-
  pedirNombre(Pokemon,NombreDado),
  append(Pokemon,[NombreDado],PokemonNombrado). %le agrega a la lista Pokemon el nombre dado al final.

pedirNombre(Pokemon,NombreDado):-
  pokemonesEntrenador(TusPokemones),
  obtenerNombre(Pokemon,NombrePokemon),
  write("Como vas a llamar a tu nuevo "),write(NombrePokemon),write("?"),nl,
  leer(NombreDado),nl,
  not(member([_,_,_,_,_,_,_,_,NombreDado],TusPokemones)).
pedirNombre(Pokemon,NombreDado):-
  write("Ese nombre ya esta ocupado o no es valido, ingresa otro nombre"),nl,nl,
  pedirNombre(Pokemon,NombreDado).

% --- Actualiza Pokemon

actualizaExperiencia(Pokemon,Experiencia,NuevoPokemon):-remplazarIndice(5,Experiencia,Pokemon,NuevoPokemon).

% --- Sacar informacion de "objeto" pokemon

pokemonAtaques([_,_,_,_,_,_,_,Ataques|_],Ataques).
pokemonEstado([_,_,_,_,Estado|_],Estado).
pokemonVidaActual([_,_,_,VidaActual|_],VidaActual).
pokemonVidaMax([_,_,VidaMax|_],VidaMax).
pokemonNivel([_,_,_,_,_,_,Nivel|_],Nivel).
pokemonTipo([_,Tipo|_],Tipo).
pokemonExperiencia([_,_,_,_,_,Experiencia|_],Experiencia).
obtenerNombre([Nombre|_],Nombre). %obtiene el nombre de pokehuevo/pokebola/pokemon
nombreMiPokemon([_,_,_,_,_,_,_,_,Nombre|_],Nombre). %obtiene el nombre dado al pokemon

% --- actualizar attributo

actualizarNivel(Pokemon,Nivel,PokemonActualizado):-remplazarIndice(6,Nivel,Pokemon,PokemonActualizado).
actualizarNombre(Pokemon,Nombre,PokemonActualizado):-remplazarIndice(0,Nombre,Pokemon,PokemonActualizado).


%  --- genera lista con N pokemones random (no repetibles, N tiene que ser menor a total de pokemones)

pokemonesRandom(Cantidad,PokemonesRandom):-pokemonesRandom(Cantidad,1,PokemonesRandom).
pokemonesRandom(Cantidad,Nivel,PokemonesRandom):-
  pokemones(Pokemones),
  random_permutation(Pokemones,PokemonesMezclados),
  pokemonesRandom(Cantidad,Nivel,PokemonesMezclados,PokemonesRandom).

pokemonesRandom(0,_,_,[]).
pokemonesRandom(Cantidad,Nivel,[Pokemon|PokemonesRestantes],[PokemonEvolucionado|PokemonesRandom]):-
  CantidadRestante is Cantidad - 1,
  actualizarNivel(Pokemon,Nivel,PokemonNivelActualizado),
  evolucionarSinMensaje(PokemonNivelActualizado,PokemonEvolucionado),
  pokemonesRandom(CantidadRestante,Nivel,PokemonesRestantes,PokemonesRandom),!.

actualizarPokehuevo(Pokehuevo,PokehuevoActualizado):-
  pokehuevosEntrenador(Pokehuevos),
  remplazarElemento(Pokehuevo,PokehuevoActualizado,Pokehuevos,PokehuevosActualizados),
  cambiarHecho(pokehuevosEntrenador(_),pokehuevosEntrenador(PokehuevosActualizados)).


sacarPokehuevoDeMochila(Pokehuevo):-
  pokehuevosEntrenador(Pokehuevos),
  nth0(_,Pokehuevos,Pokehuevo,PokehuevosActualizados),
  cambiarHecho(pokehuevosEntrenador(_),pokehuevosEntrenador(PokehuevosActualizados)).


mostrarMochila:-
  shell(clear),
  nl,write("Mochila: "),nl,nl,
  write("Pokemones Jugador: "),nl,
  mostrarPokemonesEntrenador,nl,
  mostrarPokehuevos,nl,
  mostrarDinero,nl,
  mostrarPokebolas,nl,
  mostrarMedallas,nl,nl,
  esperarRespuesta1Enter,
  shell(clear).


mostrarPokehuevos:-
  pokehuevosEntrenador(Pokehuevos),
  Pokehuevos=[_|_],nl, % lista no esta vacia
  write("Pokehuevos:"),nl,
  forall(member(Pokehuevo,Pokehuevos),mostrarPokehuevo(Pokehuevo)).
mostrarPokehuevos:-write("No tienes ningun pokehuevo."),nl.

mostrarPokehuevo([Tipo,DistanciaFaltante]):-
  tab(5),write("Pokehuevo de tipo: "),write(Tipo),write(", Distancia necesaria: "),write(DistanciaFaltante),nl.

mostrarMedallas:-
  write("Medallas: "),nl,
  ciudades(Ciudades),
  forall(member(Ciudad,Ciudades),mostrarMedalla(Ciudad)).

mostrarMedalla(Ciudad):-
  medallasEntrenador(Medallas),
  member(Ciudad,Medallas),
  tab(5),write("Conseguiste la medalla del gimnasio "),write(Ciudad),nl.
mostrarMedalla(Ciudad):-
  tab(5),write("Falta conseguir la medalla del gimnasio "),write(Ciudad),nl.

mostrarDinero:-
  dineroEntrenador(Dinero),
  write("Dinero: "),write(Dinero),write(" PokePesos"),nl.

mostrarPokebolas:-
  write("Pokebolas Jugador:"),nl,
  pokebolas(Pokebolas),
  forall(member([Tipo|_],Pokebolas),mostrarCantidadDePokebola(Tipo)).
mostrarCantidadDePokebola(Tipo):-
  pokebolasEntrenador(PokebolasEntrenador),
  contarInstancias([Tipo|_],PokebolasEntrenador,0,Cantidad),
  tab(5),write("Tienes "),write(Cantidad),write(" pokebolas tipo "),write(Tipo),nl.

mostrarBarrasDeVida:-
  shell(clear),
  pokemonActivo(PokemonActivo),
  pokemonEnemigo(PokemonEnemigo),
  mostrarNombres(PokemonActivo,PokemonEnemigo),
  mostrarBarras(PokemonActivo,PokemonEnemigo),nl.

mostrarNombres(PokemonActivo,PokemonEnemigo):-
  nombreMiPokemon(PokemonActivo,Nombre),
  obtenerNombre(PokemonEnemigo,NombreEnemigo),
  string_length(Nombre,LongitudNombre),
  string_length(NombreEnemigo,LongitudNombreEnemigo),
  vista(AnchuraVista,_),
  CaracteresAnchura is AnchuraVista * 3,
  EspaciosEntreNombres is CaracteresAnchura - LongitudNombre - LongitudNombreEnemigo,
  write(Nombre),tab(EspaciosEntreNombres),write(NombreEnemigo),nl.

mostrarBarras(PokemonActivo,PokemonEnemigo):-
  vista(AnchuraVista,_),
  AnchuraCaracteres is AnchuraVista * 3,
  AnchuraBarra is floor(AnchuraCaracteres/3),
  EspaciosEntreNombres is AnchuraCaracteres - 4 - (AnchuraBarra *2),
  mostrarBarraJugador(PokemonActivo,AnchuraBarra),
  tab(EspaciosEntreNombres),
  mostrarBarraEnemigo(PokemonEnemigo,AnchuraBarra),nl.

mostrarBarraJugador(PokemonActivo,AnchuraBarra):-
  pokemonVidaActual(PokemonActivo,Vida),
  pokemonVidaMax(PokemonActivo,VidaMax),
  write("|"),
  Porcentaje is ceil((Vida/VidaMax)*AnchuraBarra),
  forall(between(1,AnchuraBarra,X),
  (
    (X=<Porcentaje,write("\\"));
    (write("_"))
  )),
  write("|").
mostrarBarraEnemigo(PokemonEnemigo,AnchuraBarra):-
  pokemonVidaActual(PokemonEnemigo,Vida),
  pokemonVidaMax(PokemonEnemigo,VidaMax),
  write("|"),
  Porcentaje is AnchuraBarra-ceil((Vida/VidaMax)*AnchuraBarra),
  forall(between(1,AnchuraBarra,X),
  (
    (X<Porcentaje,write("_"));
    (write("/"))
  )),
  write("|").

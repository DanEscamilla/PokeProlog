% TODO
% limpiar la impresion de mensajes con shelel(clear) y esperarRespuesta
% unir mapa ciudad a pokemon.pl.
% agregar tipo a ataques.
% mostrar barras de vida (con animaciones).
:-ensure_loaded("funciones-genericas.pl").
:-ensure_loaded("mapa.pl").
:-ensure_loaded("hechos.pl").










% JUEGO EMPIEZA AQUI
jugarPokemon:-
  init,
  elegirPokemonInicial,
  enCiudad.

enCiudad:-abrirMapaCiudad.

% elegirAccionEnCiudad:-
%   nl,elegirOpcion("A donde deseas ir?",["Ir a hospital","Ir a gimnasio","Ir a tienda","Inspeccionar Mochila","Ir a otra ciudad"],OpcionElegida),nl,
%   posibilidadesCiudad(OpcionElegida).

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
% llendoACiudad:-
%   posibilidadesCamino,
%   llegarACiudad,
%   enCiudad.







%-------------------ESPECIFICAS POKEMON-------------------



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
  elegirOpcion("Deseas deseas retar al lider de gimnasio?",[retar,salir],OpcionElegida),
  accionEntrarGimasio(OpcionElegida).
accionEntrarGimasio:-accionEntrarGimasio.

accionEntrarGimasio(retar):-iniciarPeleaGimnasio.
accionEntrarGimasio(salir):-
  write("Saliste del gimnasio"),nl,nl.



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
  write("Vuelva pronto!"),nl,nl.
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
  elegirOpcion("Deseas aceptar el duelo o rechazarlo?",[aceptar,rechazar],OpcionElegida),
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
  write("El entrenador enemigo eligio su pokemon! "),nl,
  mostrarPokemonEnemigo(Pokemon),nl,
  cambiarHecho(pokemonEnemigo(_),pokemonEnemigo(Pokemon)),
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
  Pokemon = [Nombre|_],
  nl,write("Encontraste un "),write(Nombre),write(" salvaje!"),nl,
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
  elegirOpcion("Deseas pelear o correr?",[pelear,correr],OpcionElegida),
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
  nl,write("Que pokemon vas a utilizar para la pelea?"),nl,nl,
  elegirTuPokemon(Pokemon),
  establezerPokemonParaPelea(Pokemon).

establezerPokemonParaPelea(Pokemon):-
  pokemonEstado(Pokemon,Estado),
  Estado \= caido,
  cambiarHecho(pokemonActivo(_),pokemonActivo(Pokemon)).

establezerPokemonParaPelea(Pokemon):-
  nombreMiPokemon(Pokemon,Nombre),
  write(Nombre),write(" esta caido, para poder utilizarlo debes revivirlo en el hospital pokemon."),nl,nl,
  elegirPokemonParaPelea.

tienesPokemonesVivos:-
  pokemonesEntrenador(Pokemones),
  member(Pokemon,Pokemones),
  estaVivo(Pokemon).

% --- pelea ---

pelea:-ataqueJugador.

ataqueJugador:-
  pokemonActivo(Pokemon),
  pokemonEnemigo(PokemonEnemigo),
  estaVivo(Pokemon),
  elegirAtaque(Ataque),!,
  calcularPoderAtaque(Pokemon,Ataque,PoderAtaque),
  calcularEfectividad(Pokemon,PokemonEnemigo,PoderAtaque,PoderAtaqueModificado),
  atacarPokemon(PoderAtaqueModificado,PokemonEnemigo,PokemonEnemigoLastimado),
  cambiarHecho(pokemonEnemigo(_),pokemonEnemigo(PokemonEnemigoLastimado)),
  nombreMiPokemon(Pokemon,Nombre),
  obtenerNombre(PokemonEnemigo,NombreEnemigo),
  pokemonVidaActual(PokemonEnemigoLastimado,VidaRestanteEnemigo),
  write(Nombre),write(" utilizo "),write(Ataque),write(", hiciste "),write(PoderAtaqueModificado),write(" de dano!"),nl,
  write("El "),write(NombreEnemigo),write(" enemigo tiene "),write(VidaRestanteEnemigo),write(" de vida restante."),nl,nl,
  ataqueEnemigo.
ataqueJugador:-terminoPelea.

ataqueEnemigo:-
  pokemonActivo(Pokemon),
  pokemonEnemigo(PokemonEnemigo),
  estaVivo(PokemonEnemigo),
  elegirAtaqueRandom(PokemonEnemigo,Ataque),
  calcularPoderAtaque(PokemonEnemigo,Ataque,PoderAtaque),
  calcularEfectividad(PokemonEnemigo,Pokemon,PoderAtaque,PoderAtaqueModificado),
  atacarPokemon(PoderAtaqueModificado,Pokemon,PokemonLastimado),
  cambiarHecho(pokemonActivo(_),pokemonActivo(PokemonLastimado)),
  nombreMiPokemon(Pokemon,Nombre),
  obtenerNombre(PokemonEnemigo,NombreEnemigo),
  pokemonVidaActual(PokemonLastimado,VidaRestante),
  write("El "),write(NombreEnemigo),write(" enemigo utilizo "),write(Ataque),write(", te hizo "),write(PoderAtaqueModificado),write(" de dano!"),nl,
  write(Nombre),write(" tiene "),write(VidaRestante),write(" de vida restante."),nl,nl,
  ataqueJugador.
ataqueEnemigo:-terminoPelea.

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

calcularEfectividad(Pokemon,PokemonEnemigo,PoderAtaque,PoderModificado):-
  pokemonTipo(Pokemon,Tipo),
  pokemonTipo(PokemonEnemigo,TipoEnemigo),
  debilidades(TipoEnemigo,Tipo,Modificador),
  mensajeEfectividad(Modificador),
  PoderModificado is PoderAtaque * Modificador.
calcularEfectividad(_,_,PoderAtaque,PoderAtaque).

mensajeEfectividad(2):-write("SUPER EFECTIVO!"),nl.
mensajeEfectividad(1.5):-write("EFECTIVO!"),nl.
mensajeEfectividad(_).

% --- terminar pelea ---


terminoPelea:-
  tab(5),write("TERMINO PELEA"),nl,nl,
  validarGanador.

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
  write(NombreDado),write(" obtuvo "),write(Incremento),write(" de experiencia. Experiencia faltante para subir nivel: "),write(ExperienciaNecesaria),nl,nl.

% Subio de nivel
aumentarExperiencia(Pokemon,Incremento,NuevoPokemon):-
  Pokemon = [Nombre,Tipo,VidaMax,_,_,Experiencia,Nivel,Ataques,NombrePersonalizado],
  ExperienciaIncrementada is Experiencia + Incremento,
  calculaExperienciaNecesaria(Nivel,ExperienciaIncrementada,ExperienciaNecesaria),
  NuevaExperiencia is -ExperienciaNecesaria, % Subio de nivel, experiencia actual es la experiencia que sobra postiva
  NuevoNivel is Nivel + 1,
  NuevaVidaMax is VidaMax + 20,
  NuevoPokemon = [Nombre,Tipo,NuevaVidaMax,NuevaVidaMax,excelente,NuevaExperiencia,NuevoNivel,Ataques,NombrePersonalizado],
  write(NombrePersonalizado),write(" HA SUBIDO DE NIVEL!!! AHORA ES NIVEL "),write(NuevoNivel),nl,nl.

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
  % remplazarIndice(3,100,PokemonVencido,PokemonVencido2), % debug
  % remplazarIndice(5,excelente,PokemonVencido,PokemonVencido2), % debug
  obtenerNombre(PokemonVencido,TipoPokemon),
  write("Deseas Capturar al "),write(TipoPokemon),write("?"),
  elegirOpcion("",[si,no],Opcion),
  accionCapturarPokemon(Opcion,PokemonVencido).

accionCapturarPokemon(si,Pokemon):-
  pokebolasEntrenador(PokebolasEntrenador),
  elegirOpcion("Que pokebola deseas utilizar?",PokebolasEntrenador,PokebolaElegida),
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
  write("El "),write(NombrePokemon),write(" escapo! mejor suerte la proxima"),nl.

sacarPokebolaMochila(Pokebola):-
  pokebolasEntrenador(Pokebolas),
  nth0(_,Pokebolas,Pokebola,PokebolasRestantes),!,
  cambiarHecho(pokebolasEntrenador(_),pokebolasEntrenador(PokebolasRestantes)).

% --- Encuentra pokehuevo random y agrega a mochila

encontrarPokehuevo:-
  pokehuevoRandom(Pokehuevo),
  Pokehuevo = [Nombre|_],
  nl,write("Encontraste un nuevo pokehuevo de "),write(Nombre),write("!"),nl,
  agregarPokehuevoAMochila(Pokehuevo).

pokehuevoRandom(Pokehuevo):-
  pokehuevos(Pokehuevos),
  elementoRandomLista(Pokehuevos,Pokehuevo).


% --- Encuentra pokebola random y agrega a mochila

encontrarPokebola:-
  pokerbolaRandom(Pokebola),
  Pokebola = [Nombre|_],
  nl,write("Encontraste una pokebola "),write(Nombre),write("!"),nl,
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
  pokemones(Pokemones),
  pokemonesEntrenador(PokemonesEntrenador),
  pokemonesBill(PokemonesBill),
  append(PokemonesEntrenador,PokemonesBill,TodosTusPokemones),
  obtenerNombresPokemones(TodosTusPokemones,TusPokemonesNombres),
  obtenerNombresPokemones(Pokemones,PokemonesNombres),
  list_to_set(TusPokemonesNombres,TusPokemonesSet),
  sort(TusPokemonesSet,TusPokemonesOrdenados),
  sort(PokemonesNombres,PokemonesOrdenados),
  PokemonesOrdenados = TusPokemonesOrdenados,
  write("Has capturado a todos los pokemones!!"),nl,
  write("Felicidades, Ganaste!"),nl,nl,
  abort.
checarSiCapturasteATodos.

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

elegirTuPokemon(Pokemon):-
  mostrarPokemonesEntrenador,
  leer(Eleccion),nl,
  pokemonesEntrenador(Pokemones),
  nth0(Eleccion,Pokemones,Pokemon).
elegirTuPokemon(Pokemon):-
  write("Opcion invalida"),nl,nl,
  elegirTuPokemon(Pokemon).

% -- Imprime Lista de pokemones del jugador enumerada por renglon y con el nombre que se le dio al pokemon

mostrarPokemonesEntrenador:-
  pokemonesEntrenador(Pokemones),
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

% Mochila llena, decidir accion para pokehuevo
accionNoEspacioParaPokemon(Pokehuevo):-
  mostrarOpcionesMochilaLLena("No hay espacio en mochila, Que deseas hacer con el pokemon?"),
  leer(Eleccion),nl,
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
  mostrarOpcionesMochilaLLena("No hay espacio en mochila, Que deseas hacer con el pokehuevo?"),
  leer(Eleccion),nl,
  decisionPokehuevo(Eleccion,Pokehuevo).
accionNoEspacioParaPokehuevo(Pokehuevo):-accionNoEspacioParaPokehuevo(Pokehuevo).

% posibles acciones
decisionPokehuevo(0,_). %dejarlo
decisionPokehuevo(1,Pokehuevo):- %deshacerte de un pokemon
  mandarPokemonABill,
  agregarPokehuevoAMochila(Pokehuevo). %deshacerte de un pokehuevo
decisionPokehuevo(2,Pokehuevo):-
  mandarPokehuevoABill,
  agregarPokehuevoAMochila(Pokehuevo).

% -- Muestra opciones dependiendo del estado de la mochila

mostrarOpcionesMochilaLLena(Mensaje):-
  write(Mensaje),nl,
  write("0-dejarlo"),nl,
  (mostrarOpcionMandarPokemonABill;true),
  (mostrarOpcionMandarPokehuevoABill;true).

mostrarOpcionMandarPokemonABill:- % esta opcion solo se muestra si hay mas de 1 un pokemon en mochila
  pokemonesEntrenador([_,_|_]),
  write("1-Mandar pokemon a bill"),nl.

mostrarOpcionMandarPokehuevoABill:- % esta opcion solo se muestra si hay al menos 1 pokehuevo en mochila
  pokehuevosEntrenador([_|_]),
  write("2-Mandar pokehuevo a bill"),nl.


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
  write("Que pokemon vas a mandar a bill?"),nl,
  elegirTuPokemon(Pokemon),
  mandarPokemonABill(Pokemon).

mandarPokemonABill(Pokemon):-
  pokemonesEntrenador(Pokemones),
  nth0(_,Pokemones,Pokemon,PokemonesRestantes),
  pokemonesBill(PokemonesBill),
  cambiarHecho(pokemonesBill(_),pokemonesBill([Pokemon|PokemonesBill])),
  cambiarHecho(pokemonesEntrenador(_),pokemonesEntrenador(PokemonesRestantes)),
  nombreMiPokemon(Pokemon,NombreDado),
  write("Mandaste a un "),write(NombreDado),write(" con bill!"),nl.

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
  write("POKEHUEVOS ORIGINALES "), write(Pokehuevos),nl,
  write("POKEHUEVOS Actualizados "), write(PokehuevosActualizados),nl,
  cambiarHecho(pokehuevosEntrenador(_),pokehuevosEntrenador(PokehuevosActualizados)).


mostrarMochila:-
  shell(clear),
  nl,write("Mochila: "),nl,nl,
  write("Pokemones Jugador: "),nl,
  mostrarPokemonesEntrenador,nl,
  mostrarPokehuevos,nl,
  mostrarDinero,nl,
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

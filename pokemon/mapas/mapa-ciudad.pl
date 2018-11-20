posicionJugadorCiudad(2,29).


definirMapaCiudad:-
  retractall(transicion(_)),
  cambiarHecho(mapa(_,_),mapa(80,30)),
  ciudadActual(Ciudad),
  string_concat('Ciudad ',Ciudad,NombreCiudad),
  cambiarHecho(mapaActual(_),mapaActual(NombreCiudad)),
  posicionJugadorCiudad(X,Y),
  cambiarHecho(posicionJugador(_,_),posicionJugador(X,Y)),
  cambiarHecho(jugadorSeMovio,(jugadorSeMovio:-jugadorSeMovioAdaptadorCiudad)),
  retractall(objetoEnMapa(_,_,_,_)),
  listaObjetosMapaCiudad(ObjetosMapaCiudad),
  generarObjetos(ObjetosMapaCiudad).

abrirMapaCiudad:-
  definirMapaCiudad,
  abrirMapa.

jugadorSeMovioAdaptadorCiudad:-
  posicionJugador(JugadorX,JugadorY),
  objetoEnMapa(_,JugadorX,JugadorY,TipoObjeto),
  validarColisionObjetoCiudad(TipoObjeto).
jugadorSeMovioAdaptadorCiudad.

validarColisionObjetoCiudad("salida"):-
  ciudadActual("Celeste"),
  posicionJugadorGlobal(X,_),
  cambiarHecho(posicionJugadorGlobal(_,_),posicionJugadorGlobal(X,81)),
  abrirMapaGlobal.

validarColisionObjetoCiudad("salida"):-
  abrirMapaGlobal.


listaObjetosMapaCiudad([
  ["|||",3,7,"wall"],
  ["|||",4,7,"wall"],
  ["|||",3,8,"wall"],
  ["|||",5,7,"wall"],
  ["|||",3,9,"wall"],
  ["|||",6,7,"wall"],
  ["|||",3,10,"wall"],
  ["|||",7,7,"wall"],
  ["Man",5,9,""],
  ["|||",3,11,"wall"],
  ["|||",8,7,"wall"],
  ["dar",6,9,""],
  ["Pok",5,10,""],
  ["|||",3,12,"wall"],
  ["|||",9,7,"wall"],
  ["emo",6,10,""],
  ["|||",4,12,"wall"],
  ["|||",3,13,"wall"],
  ["|||",10,7,"wall"],
  ["|||",9,8,"wall"],
  ["nes",7,10,""],
  ["|||",5,12,"wall"],
  [" * ",4,13,"mandar"],
  ["|||",3,14,"wall"],
  ["|||",11,7,"wall"],
  [" * ",10,8,"mandar"],
  ["|||",9,9,"wall"],
  ["|||",6,12,"wall"],
  [" * ",5,13,"mandar"],
  ["|||",3,15,"wall"],
  ["|||",12,7,"wall"],
  [" * ",10,9,"mandar"],
  ["|||",9,10,"wall"],
  ["|||",7,12,"wall"],
  [" * ",6,13,"mandar"],
  ["|||",3,16,"wall"],
  ["|||",13,7,"wall"],
  [" * ",10,10,"mandar"],
  ["|||",9,11,"wall"],
  ["|||",8,12,"wall"],
  [" * ",7,13,"mandar"],
  ["|||",3,17,"wall"],
  ["|||",14,7,"wall"],
  [" * ",10,11,"mandar"],
  ["|||",9,12,"wall"],
  [" * ",8,13,"mandar"],
  ["|||",3,18,"wall"],
  ["|||",15,7,"wall"],
  [" * ",14,8,"curar"],
  [" * ",10,12,"mandar"],
  [" * ",9,13,"mandar"],
  ["|||",3,19,"wall"],
  ["|||",16,7,"wall"],
  ["|||",15,8,"wall"],
  [" * ",14,9,"curar"],
  [" * ",10,13,"mandar"],
  ["|||",3,20,"wall"],
  ["|||",17,7,"wall"],
  ["|||",15,9,"wall"],
  [" * ",14,10,"curar"],
  ["|||",3,21,"wall"],
  ["|||",18,7,"wall"],
  ["|||",15,10,"wall"],
  [" * ",14,11,"curar"],
  ["|||",3,22,"wall"],
  ["|||",19,7,"wall"],
  ["Cur",17,9,""],
  ["|||",15,11,"wall"],
  [" * ",14,12,"curar"],
  ["|||",3,23,"wall"],
  ["|||",20,7,"wall"],
  ["ar ",18,9,""],
  ["Pok",17,10,""],
  ["|||",15,12,"wall"],
  [" * ",14,13,"curar"],
  ["|||",3,24,"wall"],
  ["|||",21,7,"wall"],
  ["emo",18,10,""],
  ["|||",16,12,"wall"],
  [" * ",15,13,"curar"],
  ["|||",3,25,"wall"],
  ["|||",1,27,"wall"],
  ["|||",21,8,"wall"],
  ["nes",19,10,""],
  ["|||",17,12,"wall"],
  [" * ",16,13,"curar"],
  ["|||",3,26,"wall"],
  ["|||",2,27,"wall"],
  [" * ",1,28,"salida"],
  ["|||",21,9,"wall"],
  ["|||",18,12,"wall"],
  [" * ",17,13,"curar"],
  ["|||",7,23,"wall"],
  ["|||",3,27,"wall"],
  [" * ",1,29,"salida"],
  ["|||",21,10,"wall"],
  ["|||",19,12,"wall"],
  [" * ",18,13,"curar"],
  ["|||",8,23,"wall"],
  ["|||",7,24,"wall"],
  ["___",1,30,"salida"],
  ["|||",21,11,"wall"],
  ["|||",20,12,"wall"],
  [" * ",19,13,"curar"],
  ["|||",9,23,"wall"],
  ["|||",7,25,"wall"],
  ["|||",21,12,"wall"],
  [" * ",20,13,"curar"],
  ["|||",10,23,"wall"],
  ["|||",7,26,"wall"],
  ["|||",21,13,"wall"],
  ["|||",11,23,"wall"],
  ["Hos",9,25,""],
  ["|||",7,27,"wall"],
  ["|||",21,14,"wall"],
  ["|||",12,23,"wall"],
  ["pit",10,25,""],
  ["|||",8,27,"wall"],
  ["|||",21,15,"wall"],
  ["|||",13,23,"wall"],
  ["al ",11,25,""],
  ["|||",9,27,"wall"],
  ["|||",21,16,"wall"],
  ["|||",14,23,"wall"],
  ["|||",10,27,"wall"],
  ["|||",25,13,"wall"],
  ["|||",21,17,"wall"],
  ["|||",15,23,"wall"],
  ["|||",11,27,"wall"],
  ["|||",26,13,"wall"],
  ["|||",25,14,"wall"],
  ["|||",21,18,"wall"],
  ["|||",16,23,"wall"],
  ["|||",12,27,"wall"],
  ["|||",27,13,"wall"],
  ["|||",25,15,"wall"],
  ["|||",21,19,"wall"],
  ["|||",17,23,"wall"],
  ["|||",13,27,"wall"],
  ["|||",28,13,"wall"],
  ["|||",25,16,"wall"],
  ["|||",21,20,"wall"],
  ["|||",18,23,"wall"],
  ["|||",14,27,"wall"],
  ["|||",29,13,"wall"],
  ["|||",25,17,"wall"],
  ["|||",21,21,"wall"],
  ["|||",19,23,"wall"],
  ["|||",15,27,"wall"],
  ["|||",30,13,"wall"],
  ["|||",26,17,"wall"],
  ["|||",25,18,"wall"],
  ["|||",21,22,"wall"],
  ["|||",20,23,"wall"],
  ["|||",16,27,"wall"],
  ["|||",31,13,"wall"],
  ["|||",27,17,"wall"],
  [" * ",26,18,"comprar"],
  ["|||",25,19,"wall"],
  ["|||",21,23,"wall"],
  ["|||",17,27,"wall"],
  ["|||",32,13,"wall"],
  ["|||",28,17,"wall"],
  [" * ",27,18,"comprar"],
  ["|||",25,20,"wall"],
  ["|||",18,27,"wall"],
  ["|||",33,13,"wall"],
  ["Com",31,15,""],
  ["|||",29,17,"wall"],
  [" * ",28,18,"comprar"],
  ["|||",25,21,"wall"],
  ["|||",19,27,"wall"],
  ["|||",34,13,"wall"],
  ["pra",32,15,""],
  ["|||",30,17,"wall"],
  [" * ",29,18,"comprar"],
  ["|||",25,22,"wall"],
  ["|||",20,27,"wall"],
  ["|||",35,13,"wall"],
  ["r  ",33,15,""],
  ["|||",31,17,"wall"],
  [" * ",30,18,"comprar"],
  ["|||",25,23,"wall"],
  ["|||",21,27,"wall"],
  ["|||",36,13,"wall"],
  ["|||",32,17,"wall"],
  [" * ",31,18,"comprar"],
  ["|||",25,24,"wall"],
  ["|||",22,27,"wall"],
  ["|||",47,3,"wall"],
  ["|||",37,13,"wall"],
  ["|||",33,17,"wall"],
  [" * ",32,18,"comprar"],
  ["|||",25,25,"wall"],
  ["|||",23,27,"wall"],
  ["|||",48,3,"wall"],
  ["|||",47,4,"wall"],
  ["|||",38,13,"wall"],
  ["|||",34,17,"wall"],
  [" * ",33,18,"comprar"],
  ["|||",25,26,"wall"],
  ["|||",24,27,"wall"],
  ["|||",49,3,"wall"],
  ["|||",47,5,"wall"],
  ["|||",39,13,"wall"],
  ["|||",35,17,"wall"],
  [" * ",34,18,"comprar"],
  ["|||",25,27,"wall"],
  ["|||",50,3,"wall"],
  ["|||",47,6,"wall"],
  ["|||",39,14,"wall"],
  ["|||",36,17,"wall"],
  [" * ",35,18,"comprar"],
  ["|||",26,27,"wall"],
  ["|||",51,3,"wall"],
  ["Lid",49,5,""],
  ["|||",47,7,"wall"],
  ["|||",39,15,"wall"],
  ["|||",37,17,"wall"],
  [" * ",36,18,"comprar"],
  ["|||",27,27,"wall"],
  ["|||",52,3,"wall"],
  ["er ",50,5,""],
  [" * ",49,6,"lider"],
  ["|||",47,8,"wall"],
  ["|||",39,16,"wall"],
  ["|||",38,17,"wall"],
  [" * ",37,18,"comprar"],
  ["|||",30,25,"wall"],
  ["|||",28,27,"wall"],
  ["|||",53,3,"wall"],
  [" * ",51,5,"lider"],
  [" * ",50,6,"lider"],
  ["|||",47,9,"wall"],
  ["|||",39,17,"wall"],
  [" * ",38,18,"comprar"],
  ["|||",30,26,"wall"],
  ["|||",29,27,"wall"],
  ["|||",54,3,"wall"],
  ["|||",47,10,"wall"],
  ["|||",39,18,"wall"],
  ["|||",30,27,"wall"],
  ["|||",55,3,"wall"],
  ["|||",47,11,"wall"],
  ["|||",39,19,"wall"],
  ["|||",56,3,"wall"],
  ["|||",47,12,"wall"],
  ["|||",39,20,"wall"],
  ["|||",34,25,"wall"],
  ["|||",57,3,"wall"],
  ["|||",52,8,"wall"],
  ["|||",47,13,"wall"],
  ["|||",39,21,"wall"],
  ["|||",34,26,"wall"],
  ["|||",58,3,"wall"],
  ["|||",53,8,"wall"],
  ["|||",52,9,"wall"],
  ["|||",47,14,"wall"],
  ["|||",39,22,"wall"],
  ["Tie",36,25,""],
  ["|||",34,27,"wall"],
  ["|||",59,3,"wall"],
  ["|||",54,8,"wall"],
  ["|||",52,10,"wall"],
  ["|||",47,15,"wall"],
  ["|||",39,23,"wall"],
  ["nda",37,25,""],
  ["|||",35,27,"wall"],
  ["|||",60,3,"wall"],
  ["|||",55,8,"wall"],
  ["|||",52,11,"wall"],
  ["|||",47,16,"wall"],
  ["|||",39,24,"wall"],
  ["|||",36,27,"wall"],
  ["|||",61,3,"wall"],
  ["|||",56,8,"wall"],
  ["|||",52,12,"wall"],
  ["|||",47,17,"wall"],
  ["|||",39,25,"wall"],
  ["|||",37,27,"wall"],
  ["|||",62,3,"wall"],
  ["|||",57,8,"wall"],
  ["|||",52,13,"wall"],
  ["|||",47,18,"wall"],
  ["|||",39,26,"wall"],
  ["|||",38,27,"wall"],
  ["|||",63,3,"wall"],
  ["|||",58,8,"wall"],
  ["|||",52,14,"wall"],
  ["|||",47,19,"wall"],
  ["|||",39,27,"wall"],
  ["|||",64,3,"wall"],
  ["|||",59,8,"wall"],
  ["|||",52,15,"wall"],
  ["|||",47,20,"wall"],
  ["|||",40,27,"wall"],
  ["|||",65,3,"wall"],
  ["|||",60,8,"wall"],
  ["|||",52,16,"wall"],
  ["|||",47,21,"wall"],
  ["|||",41,27,"wall"],
  ["|||",66,3,"wall"],
  ["|||",61,8,"wall"],
  ["|||",52,17,"wall"],
  ["|||",47,22,"wall"],
  ["|||",42,27,"wall"],
  ["|||",67,3,"wall"],
  ["|||",62,8,"wall"],
  ["|||",53,17,"wall"],
  ["|||",48,22,"wall"],
  ["|||",43,27,"wall"],
  ["|||",68,3,"wall"],
  ["|||",63,8,"wall"],
  ["|||",54,17,"wall"],
  ["|||",49,22,"wall"],
  ["|||",44,27,"wall"],
  ["|||",69,3,"wall"],
  ["|||",64,8,"wall"],
  ["|||",55,17,"wall"],
  ["|||",50,22,"wall"],
  ["|||",45,27,"wall"],
  ["|||",70,3,"wall"],
  ["|||",65,8,"wall"],
  ["|||",56,17,"wall"],
  ["|||",51,22,"wall"],
  ["|||",46,27,"wall"],
  ["|||",71,3,"wall"],
  ["|||",66,8,"wall"],
  ["|||",57,17,"wall"],
  ["|||",52,22,"wall"],
  ["|||",47,27,"wall"],
  ["|||",72,3,"wall"],
  ["|||",67,8,"wall"],
  ["|||",58,17,"wall"],
  ["|||",53,22,"wall"],
  ["|||",48,27,"wall"],
  ["|||",73,3,"wall"],
  ["|||",68,8,"wall"],
  ["|||",59,17,"wall"],
  ["|||",54,22,"wall"],
  ["Gim",51,25,""],
  ["|||",49,27,"wall"],
  ["|||",74,3,"wall"],
  ["|||",69,8,"wall"],
  ["|||",60,17,"wall"],
  ["|||",55,22,"wall"],
  ["nas",52,25,""],
  ["|||",50,27,"wall"],
  ["|||",75,3,"wall"],
  ["|||",70,8,"wall"],
  ["|||",61,17,"wall"],
  ["|||",56,22,"wall"],
  ["io ",53,25,""],
  ["|||",51,27,"wall"],
  ["|||",75,4,"wall"],
  ["|||",70,9,"wall"],
  ["|||",62,17,"wall"],
  ["|||",56,23,"wall"],
  ["|||",52,27,"wall"],
  ["|||",75,5,"wall"],
  ["|||",70,10,"wall"],
  ["|||",63,17,"wall"],
  ["|||",56,24,"wall"],
  ["|||",53,27,"wall"],
  ["|||",75,6,"wall"],
  ["|||",70,11,"wall"],
  ["|||",64,17,"wall"],
  ["|||",56,25,"wall"],
  ["|||",54,27,"wall"],
  ["|||",75,7,"wall"],
  ["|||",70,12,"wall"],
  ["|||",65,17,"wall"],
  ["|||",56,26,"wall"],
  ["|||",55,27,"wall"],
  ["|||",75,8,"wall"],
  ["|||",70,13,"wall"],
  ["|||",66,17,"wall"],
  ["|||",56,27,"wall"],
  ["|||",75,9,"wall"],
  ["|||",70,14,"wall"],
  ["|||",67,17,"wall"],
  ["|||",62,22,"wall"],
  ["|||",75,10,"wall"],
  ["|||",70,15,"wall"],
  ["|||",68,17,"wall"],
  ["|||",63,22,"wall"],
  ["|||",62,23,"wall"],
  ["|||",75,11,"wall"],
  ["|||",70,16,"wall"],
  ["|||",69,17,"wall"],
  ["|||",64,22,"wall"],
  ["|||",62,24,"wall"],
  ["|||",75,12,"wall"],
  ["|||",70,17,"wall"],
  ["|||",65,22,"wall"],
  ["|||",62,25,"wall"],
  ["|||",75,13,"wall"],
  ["|||",66,22,"wall"],
  ["|||",62,26,"wall"],
  ["|||",75,14,"wall"],
  ["|||",67,22,"wall"],
  ["|||",62,27,"wall"],
  ["|||",75,15,"wall"],
  ["|||",68,22,"wall"],
  ["|||",63,27,"wall"],
  ["|||",75,16,"wall"],
  ["|||",69,22,"wall"],
  ["|||",64,27,"wall"],
  ["|||",75,17,"wall"],
  ["|||",70,22,"wall"],
  ["|||",65,27,"wall"],
  ["|||",75,18,"wall"],
  ["|||",71,22,"wall"],
  ["|||",66,27,"wall"],
  ["|||",75,19,"wall"],
  ["|||",72,22,"wall"],
  ["|||",67,27,"wall"],
  ["|||",75,20,"wall"],
  ["|||",73,22,"wall"],
  ["|||",68,27,"wall"],
  ["|||",75,21,"wall"],
  ["|||",74,22,"wall"],
  ["|||",69,27,"wall"],
  ["|||",75,22,"wall"],
  ["|||",70,27,"wall"],
  ["|||",71,27,"wall"],
  ["|||",72,27,"wall"],
  ["Sal",75,25,""],
  ["|||",73,27,"wall"],
  ["ida",76,25,""],
  ["|||",74,27,"wall"],
  ["|||",75,27,"wall"],
  ["=> ",78,25,""],
  ["|||",76,27,"wall"],
  ["|||",77,27,"wall"],
  ["|||",78,27,"wall"],
  ["|||",79,27,"wall"],
  ["|||",80,27,"wall"],
  [" * ",80,28,"salida"],
  [" * ",80,29,"salida"],
  ["___",80,30,"salida"]
]).

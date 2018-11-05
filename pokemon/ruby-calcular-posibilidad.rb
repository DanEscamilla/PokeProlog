
probabilidades = [
  ["nada",0.90],
  ["pokehuevo",0.1*0.1],
  ["pokebola",0.1*0.1],
  ["pokemon",0.1*0.6],
  ["entrenador",0.1*0.2],
]

acumulador = 0
probabilidades.each do |prob|
  puts "probabilidad acumulada para #{prob[0]} es: #{acumulador+prob[1]}"
  acumulador += prob[1]
end

# Integrantes:
# Gustavo Farani de Farias (gff)
# Thayná TODO
# Izael TODO

# Questão 1
dados = data.frame(Detalhes_de_Albuns)
print(dados)

# Questão 2
mean(dados$Qnt..de.Albuns.Vendidos)

sd(dados$Qnt..de.Albuns.Vendidos)

statMode <- function (y) {
  freq <- table(y)
  maxFreq <- max(freq)
  if (all(freq == maxFreq))
    return ("Amodal")
  else
    return (names(freq)[freq == maxFreq])
}

statMode(dados$Qnt..de.Albuns.Vendidos)

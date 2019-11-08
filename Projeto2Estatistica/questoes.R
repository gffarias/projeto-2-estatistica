# Integrantes:
# Gustavo Farani de Farias (gff)
# Thayná TODO
# Izael Effemberg da Costa (iec)

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

#Questão 5

getAlbumMaisVendidoEmUmAno <- function(ano) {
  vendas = max(dados[dados$Ano == ano,][[5]])
  album = dados[dados[5] == vendas,]["Album"]
  banda = dados[dados[[5]] == vendas,]["Artista"]
  return (merge(album, banda))
}

getAlbumMenosVendidoEmUmAno <- function(ano) {
  vendas = min(dados[dados$Ano == ano,][[5]])
  album = dados[dados[5] == vendas,]["Album"]
  banda = dados[dados[5] == vendas,]["Artista"]
  return (merge(album, banda))
}

getAlbuns <- function(ano) {
  maisVendidoEmUmAno = getAlbumMaisVendidoEmUmAno(ano)
  menosVendidoEmUmAno = getAlbumMenosVendidoEmUmAno(ano)
  return(rbind(maisVendidoEmUmAno, menosVendidoEmUmAno))
}

print(getAlbuns(2018))



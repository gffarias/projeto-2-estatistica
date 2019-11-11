# Integrantes:
# Gustavo Farani de Farias (gff)
# Thayná Cavalcante (tecs)
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

#Questao 3:
names <- c(Detalhes_de_Albuns_Página1$`Artista`)
dates <- c(Detalhes_de_Albuns_Página1$`Ano`)
publish2018 <- c()
publish2019 <- c()
contador = 1

for(x in names){
  if(dates[contador] == 2018) {
    publish2018 = c(publish2018, x)
    
  }else
    publish2019 = c(publish2019, x)
  contador = contador + 1
}
publish2018 = unique(publish2018, incomparables = FALSE)
publish2019 = unique(publish2019, incomparables = FALSE)
resposta = c(match(publish2018, publish2019))

for(i in resposta){
  if(!is.na(i))
    print(publish2019[i])
}

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

# Questão 7

artistas <- dados$Artistas
artistasVerificados <- c()
empresas <- dados$Empresas
empresasVerificadas <- c()
quantidadeDeArtistasPorEmpresas <- c()
# remove os artistas repetidos
for (i in artistas){
  if(!is.element(artistas[i], artistasVerificados)){
    artistasVerificados <- c(artistasVerificados, artistas[i])
    empresasVerificadas <- c(empresasVerificadas, empresas[i])
  }
}

todasAsEmpresas = unique(dados$Empresas)

getTotalDeArtistasPorEmpresa <- function(empresa){
  total <- 0
  for (i in artistasVerificados) {
    if (empresa == empresaVerificada[i]){
      total = total + 1
    }
  }
  return(total)
}

# popula o vetor de quantidade de artistas por empresas
for (i in todasAsEmpresas) {
  quantidadeDeArtistasPorEmpresas <- c(quantidadeDeArtistasPorEmpresas, getTotalDeArtistasPorEmpresa(todasAsEmpresas[i]))
}

numeroDeArtistasPorEmpresa <- data.frame(EMPRESAS = todasAsEmpresas, NUMERO_DE_ARTISTAS = quantidadeDeArtistasPorEmpresas)
numeroDeArtistasPorEmpresa <- order(numeroDeArtistasPorEmpresa$NUMERO_DE_ARTISTAS)

print(numeroDeArtistasPorEmpresa)
cat("\n")

#Questao 6:
names <- c(Detalhes_de_Albuns_Página1$`Artista`)
year <- c(Detalhes_de_Albuns_Página1$`Ano`)
data <- list()

unity = 0
contador = 0
contador2 = 1

for(i in names){
  for(j in names){
    if(j == i)
      contador = contador + 1
  }
  if(contador == 1){
    unity = unity + 1
    data[unity] <- paste("Artista:", i, "||", "Ano:", year[contador2], sep = " ", collapse = NULL) #Concatena e insere
  }
  contador2 <- contador2 + 1
  contador <- 0
}
data

#Questao 9:
empresa <- c(Detalhes_de_Albuns$`Empresa`)
album <- c(Detalhes_de_Albuns$`Album`)
vendas <- c(Detalhes_de_Albuns$`Qnt. de Albuns Vendidos`)
check <- c()

contador = 1
marcador = 0
maisVendido = 0
for(x in empresa){                                  #Varre o array de empresas
  if(x %in% check == FALSE){                        #checa se aquela empresa ja foi propagada
    for(y in empresa){                              #propaga o elemento corrente de X
      if(y == x){                                   #Se ele encontrou a empresa q ta propagando
        if(vendas[contador] > maisVendido){         #Checa se o valor de vendas relacionado eh maior do que o que ta guardado
          maisVendido = vendas[contador]
          data = album[contador]
        }
      }
      contador = contador + 1
    }
    print("Empresa:")
    print(x) 
    print("Album mais vendido:") 
    print(data)
    cat("\n")
    
    check <- c(check, x)
    maisVendido = 0
    contador = 1
  }
}


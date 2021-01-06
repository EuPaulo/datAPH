library(shiny)
library(lubridate)
#devtools::install_github("RamiKrispin/coronavirus")
#library(coronavirus)
library(readr)
library(RCurl)

#Controla o número mínimo de casos para o gráfico temporal
minTemporal <- 10


casosBrasil <- read.csv('dados/casosBrasil.csv')
obitosBrasil <- read.csv('dados/obitosBrasil.csv')
dadosBrasil <- merge(casosBrasil, obitosBrasil, by = 'Estado')
dadosBrasil <- dadosBrasil[dadosBrasil$Estado != 'TOTAL',]

# dadosBrasil <- dadosBrasil[2:nrow(dadosBrasil),]



#------------------ Parameters ------------------
corSuspeito <- "yellow"
corAtivo <- "blue"
corRecuperado <- "green"
corMorto <- "black"
#-----------------------------------------------


dadosGit <- getURL("https://raw.githubusercontent.com/EuPaulo/CovidDF/master/boletimDF.csv")
dadosDF <- read_csv(dadosGit)
dadosDF$Data <- as.Date(dadosDF$Data)
  
#dadosDF <- read_csv('dados/boletimDF.csv')
#dadosDF <- read_sheet('https://docs.google.com/spreadsheets/d/1Nc_JFHnVtLcxzY2sMft4oo9tvVWKy0pd7r86ozuNm5w/edit#gid=0')
dadosDF$Data <- dmy(dadosDF$Data)
inicioRAs <- which(names(dadosDF) == 'Águas Claras')
finalRAs <-  which(names(dadosDF) == 'outraUF')
dadosRA <- data.frame(t(dadosDF[nrow(dadosDF),inicioRAs:finalRAs]))
dadosRA <- cbind(dadosRA, row.names(dadosRA))
names(dadosRA) <- c('casos', 'RA')

latLongRA <- read.table(text ='
RA latitude longitude
"Plano Piloto"  -15.794258 -47.882809 
"Lago Sul"  -15.831613 -47.875754 
"Sudoeste" -15.798438 -47.924140 
"Águas Claras"  -15.841747 -48.027417 
"Taguatinga" -15.834449 -48.058903
"Guará"  -15.823860 -47.976345 
"Lago Norte"  -15.734570 -47.863487 
"Cruzeiro" -15.791505 -47.938036
"Gama" -16.016804 -48.062693
"Sobradinho" -15.650035 -47.784906
"Planaltina" -15.621994 -47.653411 
"Núcleo Bandeirante" -15.871228 -47.970243 
"Jardim Botânico" -15.864718 -47.790509 
"Paranoá" -15.775775 -47.779753 
"Samambaia" -15.877543 -48.088025 
"Riacho Fundo" -15.882552 -48.017902 
"Vicente Pires" -15.814278 -48.016027 
"Park Way" -15.904807 -47.962882
"Ceilândia" -15.819632 -48.108908 
"Recanto das emas"  -15.916568 -48.063807 
"Varjão" -15.712607 -47.876183 
"Candangolândia" -15.850333 -47.951061
"Riacho Fundo II" -15.904334 -48.047389
"Estrutural" -15.784717 -47.980088 
"SAAN" -15.762168 -47.938430
"Fercal" -15.604180 -47.870018
"Sobradinho II" -15.646135 -47.826624
"Santa Maria" -16.020599 -48.013420
"Itapoã" -15.751745 -47.769314
"São Sebastião" -15.905919 -47.771632
"Brazlândia" -15.685392 -48.196021
  ', h = T)


porDia<- c(dadosDF$`Casos confirmados`[-1], dadosDF$`Casos confirmados`[nrow(dadosDF)])-dadosDF$`Casos confirmados`
porDia <- porDia[-length(porDia)]
porDia <- c(0, porDia)



library(reshape2)
d <- dadosDF[26:nrow(dadosDF),c(1, inicioRAs:finalRAs)]
dMod <- melt(d, id = 1, value.name = 'casos')
names(dMod)[2] <- 'RA'
dMod <- merge(dMod, latLongRA, by = 'RA')
dMod$casos[is.na(dMod$casos)] <- 0
da <- dMod[rep(row.names(dMod), dMod$casos),]
situacao <- rep('infectado',nrow(da))
dadosMapa <- cbind(da, situacao)

set.seed(1)
alea <- rnorm(nrow(dadosMapa), 0.00001, 0.00050)
dadosMapa$latitude <- dadosMapa$latitude+alea
dadosMapa$longitude <- dadosMapa$longitude + rev(alea)

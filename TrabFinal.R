#Lendo a tabela e tratando os dados
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.table(con, header = FALSE, sep = ";", fill = TRUE, col.names = names)
#cepagri


#Armazenando as linhas preenchidas com NA
cepagriErro <- cepagri[is.na(cepagri[ , 5]), ]
nrow(cepagriErro)

#Eliminando as linha consideradas com erro do dataset inicial
cepagri <- cepagri[!is.na(cepagri[ , 5]), ]

#Por conta das linhas com erro, as duas primeiras colunas estão como factor, portanto
#iremos convertê-las
cepagri[ ,1] <- as.POSIXct(as.character(cepagri[,1]),
                           format = '%d/%m/%Y-%H:%M')

cepagri[ , 2] <- as.character(cepagri[ , 2])
cepagri[ , 2] <- as.numeric(cepagri[ , 2])

class(cepagri[ ,1])
class(cepagri[ ,2])

#Filtrando considerando apenas o período solicitado no trabalho
cepagri <- cepagri[cepagri[ , 1] > "2014-12-31 23:59" & 
                 cepagri[ , 1] < "2019-01-01 00:00" , ]



#Execucanto o comando Summary, percebemos que existem temperaturas  iguais a 
#99.90, na qual tudo indica que ouve um erro, portanto, devemos eliminá-las
summary(cepagri[ , 5])
cepagri <- cepagri[cepagri[ , 5] != 99.9, ]
summary(cepagri[ , 5])

#Analisando casos que a temperatura se mantém constante por um longo período
consecutive <- function(vector, k = 1) {
  n <- length(vector)
  result <- logical(n)
  for (i in (1+k):n)
    if (all(vector[(i-k):(i-1)] == vector[i]))
      result[i] <- TRUE
  for (i in 1:(n-k))
    if (all(vector[(i+1):(i+k)] == vector[i]))
      result[i] <- TRUE
  return(result)
}

#Por 24 horas
cepagri <- cepagri[!consecutive(cepagri$temp, 144),]

#Gráficos
library(ggplot2)

#Temperatura média/ano

#Media em 2015
Temp2015 <- cepagri$temp[((cepagri$horario > "2014-12-31 23:59") == "TRUE")
                       & ((cepagri$horario < "2016-01-01 00:00") == "TRUE")]

Temp2015 <- round(mean(Temp2015), 2)
Temp2015

#Media em 2016
Temp2016 <- cepagri$temp[((cepagri$horario > "2015-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2017-01-01 00:00") == "TRUE")]

Temp2016 <- round(mean(Temp2016), 2)
Temp2016


#Media em 2017
Temp2017 <- cepagri$temp[((cepagri$horario > "2016-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-01-01 00:00") == "TRUE")]

Temp2017 <- round(mean(Temp2017), 2)
Temp2017

#Media em 2018
Temp2018 <- cepagri$temp[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2019-01-01 00:00") == "TRUE")]

Temp2018 <- round(mean(Temp2018), 2)
Temp2018



#Umidade média/ano

#umidade em 2015
Umid2015 <- cepagri$umid[((cepagri$horario > "2014-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2016-01-01 00:00") == "TRUE")]

Umid2015 <- round(mean(Umid2015), 2)
Umid2015

#Media em 2016
Umid2016 <- cepagri$umid[((cepagri$horario > "2015-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2017-01-01 00:00") == "TRUE")]

Umid2016 <- round(mean(Umid2016), 2)
Umid2016


#Media em 2017
Umid2017 <- cepagri$umid[((cepagri$horario > "2016-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-01-01 00:00") == "TRUE")]

Umid2017 <- round(mean(Umid2017), 2)
Umid2017

#Media em 2018
Umid2018 <- cepagri$umid[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2019-01-01 00:00") == "TRUE")]

Umid2018 <- round(mean(Umid2018), 2)
Umid2018


#vento média/ano

#Media em 2015
Vento2015 <- cepagri$vento[((cepagri$horario > "2014-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2016-01-01 00:00") == "TRUE")]

Vento2015 <- round(mean(Vento2015), 2)
Vento2015

#Media em 2016
Vento2016 <- cepagri$vento[((cepagri$horario > "2015-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2017-01-01 00:00") == "TRUE")]

Vento2016 <- round(mean(Vento2016), 2)
Vento2016


#Media em 2017
Vento2017 <- cepagri$vento[((cepagri$horario > "2016-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-01-01 00:00") == "TRUE")]

Vento2017 <- round(mean(Vento2017), 2)
Vento2017

#Media em 2018
Vento2018 <- cepagri$vento[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2019-01-01 00:00") == "TRUE")]

Vento2018 <- round(mean(Vento2018), 2)
Vento2018

#Sensação Térmica média/ano

#Media em 2015
Sensa2015 <- cepagri$sensa[((cepagri$horario > "2014-12-31 23:59") == "TRUE")
                           & ((cepagri$horario < "2016-01-01 00:00") == "TRUE")]

Sensa2015 <- round(mean(Sensa2015), 2)
Sensa2015

#Media em 2016
Sensa2016 <- cepagri$sensa[((cepagri$horario > "2015-12-31 23:59") == "TRUE")
                           & ((cepagri$horario < "2017-01-01 00:00") == "TRUE")]

Sensa2016 <- round(mean(Sensa2016), 2)
Sensa2016


#Media em 2017
Sensa2017 <- cepagri$sensa[((cepagri$horario > "2016-12-31 23:59") == "TRUE")
                           & ((cepagri$horario < "2018-01-01 00:00") == "TRUE")]

Sensa2017 <- round(mean(Sensa2017), 2)
Sensa2017

#Media em 2018
Sensa2018 <- cepagri$sensa[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                           & ((cepagri$horario < "2019-01-01 00:00") == "TRUE")]

Sensa2018 <- round(mean(Sensa2018), 2)
Sensa2018


vectorTempMed <- c(Temp2015, Temp2016, Temp2017, Temp2018)
vectorUmidMed <- c(Umid2015, Umid2016, Umid2017, Umid2018)
vectorVentoMed <- c(Vento2015, Vento2016, Vento2017, Vento2018)
VectorSensaMed <- c(Sensa2015, Sensa2016, Sensa2017, Sensa2018)
vectorAnoMed <- c(2015, 2016, 2017, 2018)

dfMean = data.frame(Ano = vectorAnoMed, Temperatura = vectorTempMed, 
                        Umidade = vectorUmidMed, 
                        Vento = vectorVentoMed,
                    Sensacao = VectorSensaMed)

#Gerando gráfico da temperatura média/ano
p <- ggplot(dfMean , aes(x = Ano,
                         y = Temperatura))
p <- p + geom_point() + geom_line()

p <- p + labs(title = "Média da Temperatura por Ano"); p

p <- p + theme(plot.title =
               element_text(hjust = 0.5)); p

ggsave("mediaTempAno.png", # units = "in"
         width = 4, height = 6)

#Gráfico de vento x temperatura por ano
p <- ggplot(dfMean, aes(x = Ano)); p
p <- p + geom_point(aes(y = Temperatura,
                        colour = "Temp")); p
p <- p + geom_smooth(aes(y = Temperatura,
                        colour = "Temp")); p


p <- p + geom_point(aes(y = Vento,
                        colour = "Vento")); p
p <- p + geom_smooth(aes(y = Vento,
                       colour = "Vento")); p

p <- p + labs(colour = "Legenda:"); p

p <- p + theme(legend.background =
               element_rect(linetype = "solid")); p
p <- p + theme(legend.position = "bottom"); p
p <- p + theme(legend.position =
               c(0.85, 0.3)); p
ggsave("mediaTempVentoAno.png", # units = "in"
       width = 4, height = 4)

#Tabela
write.table(dfMean , "MediaPorAno.csv")


#Estações do Ano
#Media em 2018
Verao2018 <- cepagri$temp[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                           & ((cepagri$horario < "2018-03-01 00:00") == "TRUE")]

MaxVerao2018 <- max(Verao2018)

Out2018 <- cepagri$temp[((cepagri$horario > "2018-02-28 23:59") == "TRUE")
                          & ((cepagri$horario < "2018-06-01 00:00") == "TRUE")]

MaxOut2018 <- max(Out2018)
MaxOut2018

Inv2018 <- cepagri$temp[((cepagri$horario > "2018-05-31 23:59") == "TRUE")
                        & ((cepagri$horario < "2018-09-01 00:00") == "TRUE")]

MaxInv2018 <- max(Inv2018)
MaxInv2018
min(Inv2018)


Prim2018 <- cepagri$temp[((cepagri$horario > "2018-08-31 23:59") == "TRUE")
                        & ((cepagri$horario < "2018-11-01 00:00") == "TRUE")]

MaxPrim2018 <- max(Prim2018)
MaxPrim2018


vectorTempMax <- c(MaxVerao2018, MaxOut2018, MaxInv2018, MaxPrim2018)
vectorEstacoes <- c("Verao", "Outono", "Inverno", "Primavera")

dfEstacoes = data.frame(Estacoes = vectorEstacoes, Temperatura = vectorTempMax) 
dfVerao = data.frame(Temperatura = Verao2018)


ggplot(dfVerao, aes(x = Temperatura)) +
geom_histogram(aes(y = ..density..),
               binwidth = 5,
               boundary = 0,
               alpha = 0.5) +
  geom_density()

ggsave("TempVerao.png", # units = "in"
       width = 4, height = 4)




UmidJul2018 <- cepagri$umid[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-02-01 00:00") == "TRUE")]

dfUmid2018 <- data.frame(Umidade = UmidJul2018)
dfUmid2018$Condicao <- ifelse(dfUmid2018$Umidade > 70,
                "Ideal", "Não Ideal")
dfUmid2018


ggplot(dfUmid2018, aes(x = Umidade,
      colour = Condicao,
      fill = Condicao)) +
      geom_density(alpha = 0.25)


ggsave("umidadeIdeal.png", # units = "in"
       width = 4, height = 4)

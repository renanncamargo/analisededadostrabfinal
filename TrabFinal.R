#Lendo a tabela e tratando os dados
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.table(con, header = FALSE, sep = ";", fill = TRUE, col.names = names)

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

#Por 8h horas
cepagri <- cepagri[!consecutive(cepagri$temp, 48),]

#Gráficos
library(ggplot2)

#Temperatura média/ano

#Media da temperatura em 2015
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

#Umidade em 2016
Umid2016 <- cepagri$umid[((cepagri$horario > "2015-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2017-01-01 00:00") == "TRUE")]

Umid2016 <- round(mean(Umid2016), 2)
Umid2016


#Umidade em 2017
Umid2017 <- cepagri$umid[((cepagri$horario > "2016-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-01-01 00:00") == "TRUE")]

Umid2017 <- round(mean(Umid2017), 2)
Umid2017

#Umidade em 2018
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
MinVerao2018 <- min(Verao2018)

Out2018 <- cepagri$temp[((cepagri$horario > "2018-02-28 23:59") == "TRUE")
                          & ((cepagri$horario < "2018-06-01 00:00") == "TRUE")]

MaxOut2018 <- max(Out2018)
MinOut2018 <- min(Out2018)

Inv2018 <- cepagri$temp[((cepagri$horario > "2018-05-31 23:59") == "TRUE")
                        & ((cepagri$horario < "2018-09-01 00:00") == "TRUE")]

MaxInv2018 <- max(Inv2018)
MinInv2018 <- min(Inv2018)


Prim2018 <- cepagri$temp[((cepagri$horario > "2018-08-31 23:59") == "TRUE")
                        & ((cepagri$horario < "2018-11-01 00:00") == "TRUE")]

MaxPrim2018 <- max(Prim2018)
MinPrim2018 <- min(Prim2018)

vectorTempMax <- c(MaxVerao2018, MaxOut2018, MaxInv2018, MaxPrim2018)
vectorTempMin <- c(MinVerao2018, MinOut2018, MinInv2018, MinPrim2018)
vectorEstacoes <- c("Verao", "Outono", "Inverno", "Primavera")

dfEstacTemp2018 = data.frame(Estacoes = vectorEstacoes, Temp_Max = vectorTempMax, Temp_Min = vectorTempMin) 

write.table(dfEstacTemp2018 , "EstacTempMaxMin.csv")


#DataFrame com as temperaturas no verão
dfVerao = data.frame(Temperatura = Verao2018)


ggplot(dfVerao, aes(x = Temperatura)) +
geom_histogram(aes(y = ..density..),
               binwidth = 5,
               boundary = 0,
               alpha = 0.5) +
  geom_density()

ggsave("TempVerao.png", # units = "in"
       width = 4, height = 4)




UmidJan2018 <- cepagri$umid[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-02-01 00:00") == "TRUE")]

dfUmidJan2018 <- data.frame(Umidade = UmidJan2018)
dfUmidJan2018$Condicao <- ifelse(dfUmidJan2018$Umidade > 70,
                "Ideal", "Não Ideal")
dfUmidJan2018


ggplot(dfUmidJan2018, aes(x = Umidade,
      colour = Condicao,
      fill = Condicao)) +
      geom_density(alpha = 0.25)
ggsave("umidadeIdeal.png", width = 4, height = 4)




TempJan2018 <- cepagri$temp[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-01-28 00:00") == "TRUE")]


TempFev2018 <- cepagri$temp[((cepagri$horario > "2018-01-31 23:59") == "TRUE")
                           & ((cepagri$horario < "2018-02-28 00:00") == "TRUE")]


TempMarc2018 <- cepagri$temp[((cepagri$horario > "2018-02-28 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-03-28 00:00") == "TRUE")]

TempAbril2018 <- cepagri$temp[((cepagri$horario > "2018-03-31 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-04-28 00:00") == "TRUE")]


TempMaio2018 <- cepagri$temp[((cepagri$horario > "2018-04-30 23:59") == "TRUE")
                              & ((cepagri$horario < "2018-05-28 00:00") == "TRUE")]

TempJun2018 <- cepagri$temp[((cepagri$horario > "2018-05-31 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-06-28 00:00") == "TRUE")]


TempJul2018 <- cepagri$temp[((cepagri$horario > "2018-06-30 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-07-28 00:00") == "TRUE")]


TempAgost2018 <- cepagri$temp[((cepagri$horario > "2018-07-31 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-08-28 00:00") == "TRUE")]

TempSet2018 <- cepagri$temp[((cepagri$horario > "2018-08-31 23:59") == "TRUE")
                              & ((cepagri$horario < "2018-09-28 00:00") == "TRUE")]

TempOut2018 <- cepagri$temp[((cepagri$horario > "2018-09-30 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-10-28 00:00") == "TRUE")]


TempNov2018 <- cepagri$temp[((cepagri$horario > "2018-10-31 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-11-28 00:00") == "TRUE")]

TempDez2018 <- cepagri$temp[((cepagri$horario > "2018-11-30 23:59") == "TRUE")
                            & ((cepagri$horario < "2018-12-28 00:00") == "TRUE")]



vectorTemp2018 <- c(TempJan2018[1:3500], TempFev2018[1:3500],
                TempMarc2018[1:3500], TempAbril2018[1:3500],
                TempMaio2018[1:3500], TempJun2018[1:3500],
                TempJul2018[1:3500], TempAgost2018[1:3500],
                TempSet2018[1:3500], TempOut2018[1:3500],
                TempNov2018[1:3500], TempDez2018[1:3500])

dtTemp2018 <- NULL
dtTemp2018 <- data.frame(Temperatura = vectorTemp2018, Meses = 1)
vectorTemp2018

dtTemp2018[1:3500, 2] <- 1
dtTemp2018[3501:7000, 2] <- 2
dtTemp2018[7001:10500, 2] <- 3
dtTemp2018[10501:14000, 2] <- 4
dtTemp2018[14001:17500, 2] <- 5
dtTemp2018[17501:21000, 2] <- 6
dtTemp2018[21001:24500, 2] <- 7
dtTemp2018[24501:28000, 2] <- 8
dtTemp2018[28001:31500, 2] <- 9
dtTemp2018[31501:35000, 2] <- 10
dtTemp2018[35001:38500, ] <- 11
dtTemp2018[38501:42000, 2] <- 12

dtTemp2018$Meses <- factor(month.abb[dtTemp2018$Meses],
                   levels = month.abb,
                   ordered = TRUE)


ggplot(dtTemp2018, aes(x = Meses,
               y = Temperatura,
               group = Meses,
               fill = Meses)) + 
               geom_boxplot()

ggsave("Temp2018.png", width = 4, height = 4)

MeanTemp2018 <- aggregate(dtTemp2018$Temperatura,
          list(dtTemp2018$Meses), mean)

MeanTemp2018 <- MeanTemp2018[1:6, ]
colnames(MeanTemp2018) <- c("Meses", "Temperaturas")

write.table(MeanTemp2018 , "MediaAno2018.csv")



ggplot(dtTemp2018, aes(x = Temperatura,
                        y = Meses)) +
  geom_point(aes(colour = Temperatura),
                 alpha = 0.5) +
  scale_color_continuous(low = "yellow",
                             high = "red")
ggsave("Temp2018Vermelho.png", width = 4, height = 4)

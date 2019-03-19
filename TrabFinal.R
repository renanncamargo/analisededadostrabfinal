

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

Temp2015 <- mean(Temp2015)
Temp2015

#Media em 2016
Temp2016 <- cepagri$temp[((cepagri$horario > "2015-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2017-01-01 00:00") == "TRUE")]

Temp2016 <- mean(Temp2016)
Temp2016


#Media em 2017
Temp2017 <- cepagri$temp[((cepagri$horario > "2016-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-01-01 00:00") == "TRUE")]

Temp2017 <- mean(Temp2017)
Temp2017

#Media em 2018
Temp2018 <- cepagri$temp[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2019-01-01 00:00") == "TRUE")]

Temp2018 <- mean(Temp2018)
Temp2018



#Umidade média/ano

#umidade em 2015
Umid2015 <- cepagri$umid[((cepagri$horario > "2014-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2016-01-01 00:00") == "TRUE")]

Umid2015 <- mean(Umid2015)
Umid2015

#Media em 2016
Umid2016 <- cepagri$umid[((cepagri$horario > "2015-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2017-01-01 00:00") == "TRUE")]

Umid2016 <- mean(Umid2016)
Umid2016


#Media em 2017
Umid2017 <- cepagri$umid[((cepagri$horario > "2016-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2018-01-01 00:00") == "TRUE")]

Umid2017 <- mean(Umid2017)
Umid2017

#Media em 2018
Umid2018 <- cepagri$umid[((cepagri$horario > "2017-12-31 23:59") == "TRUE")
                         & ((cepagri$horario < "2019-01-01 00:00") == "TRUE")]

Umid2018 <- mean(Umid2018)
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



vectorTempMed <- c(Temp2015, Temp2016, Temp2017, Temp2018)
vectorUmidMed <- c(Umid2015, Umid2016, Umid2017, Umid2018)
vectorVentoMed <- c(Vento2015, Vento2016, Vento2017, Vento2018)
vectorAnoMed <- c(2015, 2016, 2017, 2018)

dfMean = data.frame(Ano = vectorAno, Temperatura = vectorTemp, 
                        Umidade = vectorUmid, 
                        Vento = vectorVentoMed)

#ggplot(dfMeanTemp , aes(x = Ano,
 #                   y = Media)) +
#geom_point() + geom_smooth()
p <- ggplot(dfMeanTemp, aes(x = Ano)); p
p <- p + geom_line(aes(y = Temperatura,
                        colour = "Temp")); p
p <- p + geom_point(); p

p <- p + geom_line(aes(y = Vento,
                       colour = "Vento")); p

p <- p + labs(colour = "Legenda:"); p
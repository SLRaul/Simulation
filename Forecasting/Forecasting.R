rm(list = ls())

library(fpp2)
library(forecast)
library(ggplot2)

# Cap 2; gráficos de séries temporais
# Exemplo dedados e a sua visualização 
data("melsyd")
autoplot(melsyd[,"Economy.Class"]) + ggtitle("Classe econômica Sydney-Melborne") +
  xlab("Ano") + ylab("Quantidade (em milhares)") +theme_bw()

data(a10)
autoplot(a10) + ggtitle("Venda de drogas anti-diabéticas") +
  ylab("$ em milhões") + xlab("Ano") +theme_bw()

# Gráficos sazonais
# year.labels = T serve para os rótulos ficarem dentro do gráfico
ggseasonplot(a10, year.labels = T, year.labels.left = T)

# em gráfico polar
ggseasonplot(a10, polar = T)

# gráficos de sub séries
ggsubseriesplot(a10) + ylab("em milhares") + xlab("Meses") +
  ggtitle("Gráfico de subsérie temporal das vendas de anti-diabético")
# a linha horizontal indica a média, esse tipo de gráfico pode revelar  udanças sazonais

# gráfico de dispersão
data("elecdemand")
# o gráfico de dispersão é idela para everiguar relações
autoplot(elecdemand) # plota com todas as variáveis
autoplot(elecdemand[,c("Demand", "Temperature")], facets=T) +# fazendo o gráfico separado
  ylab("") + xlab('Ano de 2014')

# utilizando o pacote base
plot(elecdemand, ylab = "Demand WorkDay Temperature")
plot(elecdemand[,c("Demand", "Temperature")])

# fazendo o gráfico de dispersão
qplot(Temperature, Demand, data = as.data.frame(elecdemand))
# pode-se perceber que existe uma relação entre a demana e temperatura

data("visnights")
autoplot(visnights[,c(1:5)], facets = T)+ylab("Quantidade de visitantes noturnos em milhares")

# visualizando as correlações de Pearson
GGally::ggpairs(as.data.frame(visnights[,c(1:5)]))

# gráfico LAG
data("ausbeer")
# extraindo um subset e gerando o gráfico de lag
gglagplot(window(ausbeer, start= 1992))
# no gráfico pode-se observar correlações positivas nos lags 4 e 8
# e correlações negativas nos lags 2 e 6
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

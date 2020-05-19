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
# a linha horizontal indica a média, esse tipo de gráfico pode revelar  mudanças sazonais

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

# auto correlação
ggAcf(beer) #gráfico de auto correlação #autocorrelation function
# auto correlação  dos dados iniciados em 92
ggAcf(window(ausbeer, start=1992)) + ggtitle("Consumo de cerveja desde 1992") +
  ylab("Correlação") 
# consumo de energia na Austrália
# apartir do anos 80
ggAcf(window(elec, start=1980), lag.max = 48)
autoplot(window(elec, start=1980))
# apartir de 1956
ggAcf(elec, lag.max = 48)
autoplot(elec)
# os gáficos de autocorrelação indicam tendência e sazonalidade,
# visto que observa-se uma diminuição da autocorrelação e picos de auto correlação 

# White noise - ruido branco
# Serie temporal que não mostra correlação é chamada de WN
# um ruido branco é dado por x ~ N(0,1) independentes
# por conta disso o gráfico de correlações deve ser práximo de zero
# que as correlações esteja dentro do liminte de confiança de 95%, dado por 2/sqrt(tamanho_ts)
# exemplo de WN
set.seed(2019)
gridExtra::grid.arrange(
  autoplot(ts(rnorm(50))) + ggtitle("Exememplo de WN em ST"),
  ggAcf(ts(rnorm(50))) + ggtitle("Exememplo de WN no gráfico de autocoorelação")
)
# no exe acima o limite é = 2/sqrt(50) = -ou+ 0.28

#exercícios
## 1
# a
help("gas") # Produção mensal de gas na autrália
help("woolyrnq") # Produção trimestral de de fios de lã na Austrália
help("gold") # preço diário do ouro em dollar de 1/1985 -3/89
# b
help("frequency")
# refere a frequencia anual dos dados
frequency(gas) # 12
frequency(woolyrnq) # 4
frequency(gold) # 1
# c
which.max(gold) # outlier da ST desejada
# 770

## 2
# a
tute1 <-  readr::read_csv("~/R_Diretorio/Forecast/tute1.csv")
head(tute1)
# sales = vendas trimestrais de uma empresa pequena de 1981-2005
# AdBudget = gasto com publicidade
# GPD = produto interno bruto 
# todos ajustados pela inflação
# b
tute1_ts <- ts(tute1[,-1], start = 1981, frequency = 4) # transformando em st e removendo as datas
# c
autoplot(tute1_ts, facets = T) # stde cada variável separadas
autoplot(tute1_ts, facets = F)
# possível correlação positiva entre  sales e AdBudget
# possível correlação negativa entre gdp e as demais
## 3
# a
retail <- readxl::read_excel("~/R_Diretorio/Forecast/retail.xlsx", skip = 1) 
# venda das de diversos tipos de loja e alguns estados
head(retail)
# b
retail__ts <- ts(retail[,'A3349398A'], start = c(1982,4), frequency = 12)
# c
autoplot(retail__ts)
# mostra tendencia e sazonalidade
ggseasonplot(retail__ts)
# mostra um aumento em dezembro e queda em fev
ggsubseriesplot(retail__ts)
gglagplot(window(retail__ts))
# em todos os lags existe uma correlação linear forte
ggAcf(retail__ts, lag.max= 100)
# mostra que existe uma tendência bem forte uma leve sazonalidade

## 4
?bicoal # produção anual de carvão no eua 1920-1968
?chicken # preço da galinha em dollar 1924-1993
?dole # total mensal de seguro desemprego na austrália 01/1965-06/1992
?usdeaths # mortes mensais acidentais no EUA 1973-1978
?lynx # numeros anual de lynx capturadas 1821-1934
?goog # Preços das ações do google 25/02/2013-13/02/2017
?writing # venda de papel na frança 01/1963-12/1972
?fancy # vendas mensais de souvenir no cais de um resorte em Queensland
?a10 # venda mensal de remédios para diabets na austrália 07/91-06/2008
?h02 # venda de corticoide mensal na austrália 07/1991-06/2008

autoplot(bicoal) + ylab("Quantidade produzida") + xlab("Anos") +
  ggtitle("Produção anual de carvão no EUA 1920-1968")

autoplot(chicken) + ylab("Preço em dollar") + xlab("Anos") +
  ggtitle("Série histórica do preço da galinha em dollar 1924-1993")

autoplot(dole) + ylab("Quantidade de pedidos") + xlab("Anos") +
  ggtitle("Quantidade de pedidos desempregos durante 01/1965-06/1992 na Austrália")

autoplot(usdeaths) + ylab("Quantidade de mortes") + xlab("Anos") +
  ggtitle("Quantidade de mortes acidentais no EUA 1973-1978")

autoplot(lynx) + ylab("Quantidade capturadas") + xlab("Anos") +
  ggtitle("Quantidade delynnx capturadas durante 1821-1934")

autoplot(goog) + ylab("Preço em dollar") + xlab("Anos") +
  ggtitle("Série histórica do preço das ações do Google em dollar durante 25/02/2013-13/02/2017")

autoplot(writing) + ylab("Quantidade em milhares") + xlab("anos") +
  ggtitle("Quantidade de papeis vendidos na França durante 01/1963-12/1972")

autoplot(fancy) + ylab("Quantidade vendida") + xlab("Anos") +
  ggtitle("Vendas de souvenir do Resort Queensland")

autoplot(a10) + ylab("Quantidade vendida") + xlab("Anos") +
  ggtitle("Série historica de vendas de remédios para diabetes na Austrália 07/91-06/2008")

autoplot(h02) + ylab("Quantidade vendida") + xlab("Anos") +
  ggtitle("Série historica de vendas de corticoides na Austrália 07/1991-06/2008")

## 5
gridExtra::grid.arrange(
  ggseasonplot(writing, year.labels = T),
  ggsubseriesplot(writing)
)
# Queda significativa em jul e ago, e uma recuperação em set 
# a queda é exatamente no perio de férias
gridExtra::grid.arrange(
  ggseasonplot(fancy, year.labels = T),
  ggsubseriesplot(fancy)
)
# maior venda em dez, pode estar associado as férias de natal
gridExtra::grid.arrange(
  ggseasonplot(a10, year.labels = T),
  ggsubseriesplot(a10)
)
# leve aumente em jan e "estavel no resto do ano
gridExtra::grid.arrange(
  ggseasonplot(h02, year.labels = T),
  ggsubseriesplot(h02)
)
# queda acentuada em fev e vai recuperando ao longo do ano


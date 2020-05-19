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
tute1 <- ts(tute1[,-1], start = 1981, frequency = 4) # transformando em st e removendo as datas
# c
autoplot(tute1, facets = T) # stde cada variável separadas
autoplot(tute1, facets = F)
# possível correlação positiva entre  sales e AdBudget
# possível correlação negativa entre gdp e as demais


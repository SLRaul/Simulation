#install.packages(c("survival","ggfortify", "range"))
library(survival) # para as funções de sobrevivencia
library(ggfortify) # para graficos
library(ggplot2) # para gráficos
# install_github("imbs-hl/ranger")
library(ranger) # função rápida para uso de florestas aleatórias e arvore de regressão
library(dplyr)

data(veteran)
head(veteran)

str(veteran)

# transformando em objeto survival
km <- with(veteran, Surv(time, status))

# # # # estimando por meio de Kaplan-Meier # # # #
km_ajustado <- survfit(Surv(time, status)~1, data = veteran)

# gerando a tabela com as probabilidades, numero de eventos, ...
# o parametro times serve para selecionar os tempos a serem observados
summary(km_ajustado, times = c(1,30,60,90*(1:10)))

# gerando o gráfio de sobrevivência 
# base
plot(km_ajustado)
# pacote
autoplot(km_ajustado)
# + indica censura

# gerando o gráfico de sobrevivência pelos de tratamentos
km_trat_fit <- survfit(Surv(time, status)~trt, data= veteran)
# gerando a tabela com as probabilidades, numero de eventos, ... para cada tratamento
summary(km_ajustado, times = c(1,30,60,90*(1:10)))

#gerando gráfico de sobrevivência
autoplot(km_trat_fit)
# + são referentes as censuras

# fazendo uma comparação entre os indivíduos com mais de 60 anos com os demais temos
veteran_partitioned <- veteran %>% mutate(AG = ifelse(age > 60, "L60", "OV60"))  

# gerando o gráfico de sobrevivência em relação ao grupo de idade
# pacote
 survfit(Surv(time, status)~AG, veteran_partitioned) %>% autoplot()
# comando base
 survfit(Surv(time, status)~AG, veteran_partitioned) %>% plot(col=c(1,2))
 
 # # # # estimando por meio do Cox Proportional Hazards Model # # #
 
# fonte: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
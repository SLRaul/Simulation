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
plot(km_ajustado, ylab = "Surv. Prob", xlab="Time")
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
 survfit(Surv(time, status)~AG, veteran_partitioned) %>%
   plot(col=c(1,2), xlab = "Time", ylab = "Surv. Prob.") 
   legend("topright" , legend = c("L60", "OV60"), col = c("black","red"))
 
# # # # estimando por meio do Cox Proportional Hazards Model # # #
# ajustando o modelo cox
cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior + AG, data = veteran_partitioned)
summary(cox)

cox_fit <- survfit(cox)
cox_fit %>% autoplot()

# ajustando com Aalean model
# no gráfico podemos observar como o efeito de cada covariavel muda com o tempo
aareg(Surv(time, status) ~ trt + celltype + 
    karno + diagtime + age + prior, data = veteran_partitioned) %>% 
  autoplot()

# # # # estimando por meio de floresta aleatória # # #
# modo data science de modelar evento de tempo
# com o pacote ranger

rfm <- ranger(Surv(time, status) ~ trt + celltype + 
                karno + diagtime + age + prior, data = veteran_partitioned,
              mtry= 4, # numero de variáveis para poder dividir em cada nó
              importance = "permutation", # modo de inportancia da varialvel
              splitrule = 'extratrees', # regra de divisão
              verbose = T) # mostrar os estatus dos caclulos e dasestimativas

rfm$unique.death.times
sapply(rfm$survival,mean)

# calculando o modelo sobrevivência média
death_times <- rfm$unique.death.times 
avg_prob <- sapply(data.frame(rfm$survival),mean)

# gerando o gráfico de sobrevivência para cada paciente
plot(rfm$unique.death.times,rfm$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(veteran_partitioned)[1]), 20)){
  lines(rfm$unique.death.times, rfm$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend("topright", legend = c('Average = black'))

#gerando o rank com as variáveis
rank <- data.frame(sort(rfm$variable.importance, 4, decreasing = T))
colnames(rank) <- c("Importancia"); rank



# fonte: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
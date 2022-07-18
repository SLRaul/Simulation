library(keras)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)

# selecionando os dados
numero_palavras <- 1000
imdb <- dataset_imdb(num_words = numero_palavras)

c(treino_dados, treino_labels) %<-% imdb$train
c(teste_dados, teste_labels) %<-% imdb$test

# criando a função de transformação mult-hot-enconding
multti_hot_seguencias <- function(sequencias, dimensao){
  multi_hot <- matrix(0, nrow = length(sequencias), ncol = dimensao)
  for (i in 1:length(sequencias)) {
    multi_hot[i,sequencias[[i]]] <- 1
  }
  multi_hot
}


treino_dados <- multti_hot_seguencias(treino_dados, numero_palavras)
teste_dados <- multti_hot_seguencias(teste_dados, numero_palavras)

# Vendo os resultados do multi_hot observa-se maisr frequencia de indices das palavras proxima de zero
# isso está correto pois o indice foi feito do mais frequente
primeiro_texto <- data.frame(word= 1:numero_palavras, value= treino_dados[1,])
primeiro_texto %>%  ggplot(aes(x= word, y= value)) +
  geom_line()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## Demostrando super ajuste

# modelo de base
modelo_base <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = numero_palavras) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

modelo_base %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy")
  
)

summary(modelo_base)

historia_base <- modelo_base %>% fit(
  treino_dados,
  treino_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(teste_dados, teste_labels),
  verbose = 2
)

# modelo com menos camadas
modelo_menor <- 
  keras_model_sequential() %>%
  layer_dense(units = 4, activation = "relu", input_shape = numero_palavras) %>%
  layer_dense(units = 4, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

modelo_menor %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy")
)

summary(modelo_menor)

historia_menor <- modelo_menor %>% fit(
  treino_dados,
  treino_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(teste_dados, teste_labels),
  verbose = 2
)

# modelo maior
modelo_maior <-
keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = numero_palavras) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

modelo_maior %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy")
)

summary(modelo_maior)

historia_maior <- modelo_maior %>% fit(
  treino_dados,
  treino_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(teste_dados, teste_labels),
  verbose = 2
)

# montando o plot

compare_cx <- data.frame(
  baseline_train = historia_base$metrics$loss,
  baseline_val = historia_base$metrics$val_loss,
  smaller_train = historia_menor$metrics$loss,
  smaller_val = historia_menor$metrics$val_loss,
  bigger_train = historia_maior$metrics$loss,
  bigger_val = historia_maior$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_cx, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")

# adicionando um regularizador de pesos L2

l2_modelo <- 
  keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = numero_palavras,
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%# add o peso na camada
  layer_dense(units = 16, activation = "relu",
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dense(units = 1, activation = "sigmoid")

 l2_modelo %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy")
)

l2_history <- l2_modelo %>% fit(
  treino_dados,
  treino_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(teste_dados, teste_labels),
  verbose = 2
)

# montando o plot de comparação

compare_cx <- data.frame(
  baseline_train = historia_base$metrics$loss,
  baseline_val = historia_base$metrics$val_loss,
  l2_train = l2_history$metrics$loss,
  l2_val = l2_history$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_cx, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")
# com isso observa-se que o controle de pesos l2 é mais resistente ao superajuste

# utilizando o regularizador dropout

dropout_modelo <- 
  keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = numero_palavras) %>%
  layer_dropout(0.6) %>% # add o regularizador na camada
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.6) %>%
  layer_dense(units = 1, activation = "sigmoid")

dropout_modelo %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy")
)

dropout_history <- dropout_modelo %>% fit(
  treino_dados,
  treino_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(teste_dados, teste_labels),
  verbose = 2
)

# montando o plot de comparação
compare_cx <- data.frame(
  baseline_train = historia_base$metrics$loss,
  baseline_val = historia_base$metrics$val_loss,
  dropout_train = dropout_history$metrics$loss,
  dropout_val = dropout_history$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_cx, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")

# meneiras comuns de contornar o super ajuste
# 1. ter mais dados de treinamento
# 2. reduzir a capacidade da nedeneural
# 3. add regularizador de pesos
# 4. adicionar dropout


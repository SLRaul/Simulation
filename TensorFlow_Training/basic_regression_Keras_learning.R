library(keras)
library(tfdatasets)

# carregando os dados
boston_housing <- dataset_boston_housing()

c(dados_treino, rotulos_treino) %<-% boston_housing$train
c(dados_teste, rotulos_teste) %<-% boston_housing$test

paste0('Entradas para teste:' , length(dados_treino),', Rotulos: ', length(rotulos_teste))

dados_treino[1,]

# ajustando os nomes das colunas
library(dplyr)
nome_col <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
              'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')

treino_df <- dados_treino %>% 
  as_tibble(.name_repair = 'minimal') %>% 
  setNames(nome_col) %>% 
  mutate(label= rotulos_treino)

teste_df <- dados_teste %>% 
  as_tibble(.name_repair = 'minimal') %>% 
  setNames(nome_col) %>% 
  mutate(label= rotulos_teste)

# o preço está em milhares
rotulos_treino[1:10]

# cada coluna possui uma escala diferente, por conta disso 
# será feito uma normalização dos dados

especificado <- feature_spec(treino_df, label ~ . ) %>% # especificando as colunas da tabela
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% # faz a normalização
  fit() # finaliza o processo
especificado

# com a normalização é possível usar no modelo a ser ajustado

# avaliando  a saida do/a dense-features layer criadas pelo especificado(spec)
layer <- layer_dense_features(
  feature_columns = dense_features(especificado),
  dtype = tf$float32
)
layer(treino_df) # deu erro

## criando o modelo
# preparando os dados
entrada <- layer_input_from_dataset(treino_df %>% select(-label))

# montando modelo com saida de 3 camadas
saida <- entrada %>% 
  layer_dense_features(dense_features(especificado)) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 1)

modelo <- keras_model(entrada,saida)
summary(modelo)

# Compilando
modelo %>% 
  compile(
    loss = 'mse', # mean square error
    optimizer = optimizer_rmsprop(), # RMSprop(root mean square)
    metrics = list('mean_absolut_error')
  )


# montando o codigo do modelo para reser utilizado nos experimentos
modelo_contruido <- function(){
  entrada <- layer_input_from_dataset(treino_df %>% select(-label))
  
  # montando modelo com saida de 3 camadas
  saida <- entrada %>% 
    layer_dense_features(dense_features(especificado)) %>% 
    layer_dense(units = 64, activation = 'relu') %>% 
    layer_dense(units = 64, activation = 'relu') %>% 
    layer_dense(units = 1)
  
  modelo <- keras_model(entrada,saida)
  
  # Compilando
  modelo %>% 
    compile(
      loss = 'mse', # mean square error
      optimizer = optimizer_rmsprop(), # RMSprop(root mean square)
      metrics = list('mean_absolute_error')
    )
  modelo
}

## treinando o modelo

# função para imprimindo o progresso por ponto
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

modelo_ <- modelo_contruido()

historia <- modelo_ %>% fit(
  x= treino_df %>% select(-label),
  y= treino_df$label,
  epochs= 500,
  validation_split = 0.2,
  verboose = 0,
  callback = list(print_dot_callback)
)

# contruindo o progresso do modelo para determinar quando o modelo para de fazer progresso

plot(historia)
# mostra pequenas melhorias depois da epochs 200

parada_ <- callback_early_stopping(monitor = 'val_loss',
                                   patience = 20)

modelo_ <- modelo_contruido()

historia <- modelo_ %>% fit(
  x= treino_df %>% select(-label),
  y= treino_df$label,
  epochs= 500,
  validation_split = 0.2,
  verboose = 0,
  callback = list(parada_)
)

plot(historia)

c(loss, mae) %<-% c(last(historia$metrics$val_loss), last(historia$metrics$val_mean_absolute_error))

paste0("O erro absoluto médio no teste foi: $", sprintf("%.2f", mae * 1000))

## Predição
predicao_teste <- modelo_ %>% predict(teste_df %>% select(-label))
predicao_teste[,1]

# https://tensorflow.rstudio.com/tutorials/beginners/

library(keras)

mnist <- dataset_mnist()

# convertendo de 0-255 para 0-1
mnist$train$x <- mnist$train$x/255
mnist$test$x <- mnist$test$x/255

# definindo o keras model
model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(28,28)) %>% # definindo as camadas/dimnsões
  layer_dense(units = 128, activation = 'relu') %>% # definindo dimensão da saida e o kernel de ativação 
  layer_dropout(0.2) %>% # 
  layer_dense(10, activation = 'softmax') # dimenciona a saida, função de ativação

# vendo as informaçõe das camadas, parametros, etc
summary(model)  

# compilando o modelo
#   definindo as perdas que serão otimizdas e
#   qual otimizador será usado

model2 <-
model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)
# adam = stochastic gradient descent method that is based on adaptive estimation
#   of first-order and second-order moments.

# ajustando o modelo

model %>% 
  fit(
    x= mnist$train$x, y= mnist$train$y,
    epochs = 5,
    validation_split = 0.3,
    verbose = 2
  )

# acessando a performance
model %>% 
  evaluate(mnist$test$x, mnist$test$y, verbose = 0)

# salvando  o modelo
save_model_tf(object = model,
              filepath = 'C:/Users/silva/Documents/RnDirectory/Learning')

# recarregando o modelo
reloaded_models <- load_model_tf('C:/Users/silva/Documents/RnDirectory/Learning')
# fazendo predição com o modelo carregado
all.equal(predict(model, mnist$test$x), predict(reloaded_models, mnist$test$x))

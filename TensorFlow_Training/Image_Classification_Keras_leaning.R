
library(keras)

# carregando fashion mnist
fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train

c(test_images, test_labels) %<-% fashion_mnist$test

# gabarito dos rotulos
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

# explorando os dados
dim(train_images)
train_labels
dim(test_images)
test_labels

# processando os dados
library(tidyr)
library(ggplot2)

# tratando a imagem
imagem1 <- as.data.frame(train_images[1,,])
colnames(imagem1) <- seq_len(ncol(imagem1))
imagem1$y <- seq_len(nrow(imagem1))
imagem1 <- gather(imagem1,'X', 'value', -y)
imagem1$X <- as.integer(imagem1$X)


# gerando a imagem
ggplot(imagem1, aes(x = X, y=y, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = 'white', high = 'black', na.value = NA) +
  scale_y_reverse() +
  theme_bw()+
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1)+
  xlab('')+
  ylab('')

# ajustando dados para 0-1
train_images <- train_images / 255
test_images <- test_images / 255


# vendo as primeiras 25 imagens

par(mfcol=c(5,5))
par(mar=c(0,0,1.5,0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img<- train_images[i,,]
  img<- t(apply(img, 2, rev))
  image(1:28, 1:28, img, col=gray((0:255)/255), xaxt='n', yaxt='n',
        main = paste(class_names[train_labels[i]+1]))
}

## Contruindo modelo 
# redes neurais

modelo <- keras_model_sequential()
modelo <- modelo %>% 
  layer_flatten(input_shape = c(28,28)) %>% # tranforma as imagens de um array-2d(28x28) para um array-1d(728=28x28) 
  layer_dense(units = 128, activation = 'relu') %>% # primeira camada com 128 nodes
  layer_dense(units = 10, activation = 'softmax')# segunda camada 10 nodes softmax
# a segunda camada retorna a probabilidade da imagen retornar a uma das 10 classes

## compilando o modelo
modelo <- modelo %>% compile(
  optimizer = 'adam', # o modelo base para os updates
  loss = 'sparse_categorical_crossentropy', # mede a acurárica do modelo durante o treinamento
  metrics = c('accuracy') # monitora os passos de teste e treinamento
)  

## treinano  o modelo
# passos para treinar uma rede neural
# 1. 'alimentar' os dados de treinamento do modelo
# 2. o modelo aprender a associar as imagens ao rotulo
# 3. fazer predições com o modelo

 modelo %>% fit(train_images, train_labels,
               epochs= 5, # vezes de interação entre x e y dos dados
               verbose= 2, ) # uma linha por epoch*
# *https://keras.io/api/models/model_training_apis/

## avaliando acuracidade
score <- modelo %>% evaluate(test_images,test_labels, verbose= 0)
cat('Teste de perda:', score[1])
cat('Teste de acurácia:', score[2])

## Fazendo predições
predicao <- modelo %>% predict(test_images)

# a predição e a 'confiança' do modelo na correspondência do rótulo
predicao[1,]
which.max(predicao[1,])

# vinte <- c()
# for (i in 1:length(predicao)){
#   vinte[i] <- which.max(predicao[i,])
# }
# vinte[1:20]

# alternativamento pode consultar diretamente a predição
classe_predita <- modelo %>% predict(test_images) %>% k_argmax() %>% as.vector()
classe_predita[1:20]
test_labels[1]

as.vector(classe_predita)

######################
acertou <- c()
for (i in 1:length(predicao)) {
  if (test_labels[i] == (which.max(predicao[i,])-1)) {
    acertou[i] <- 'sim'
  }else{
    acertou[i] <- 'não'
  }
}
table(acertou)
######################

# visualizando algumas classificações
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtrai 1 pois os rotulos são de 0-1
  predicted_label <- which.max(predicao[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}

## fazendo uma previsão
imagem2 <- test_images[1,,,drop = F]
dim(imagem2)

predicao2 <- modelo %>% predict(imagem2)
predicao2
which.max(predicao2[1,])-1

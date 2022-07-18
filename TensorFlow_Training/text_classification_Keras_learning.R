
library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

## carregando os dados

# https://www.kaggle.com/datasets/nltkdata/movie-review
df <- readr::read_csv("C:/Users/s***/Downloads/movie_review.csv.zip")

head(df)

# explorando os dados
df %>% count(tag)

# exemplo de review
df$text[1]

# separando os dados
treinamento_id <- sample.int(nrow(df), size = nrow(df)*0.8)
treinamento <- df[treinamento_id,]
teste <- df[-treinamento_id,]

#distribuição do numero de palavras por review
df$text %>% 
  strsplit(' ') %>% 
  sapply(length) %>% 
  summary()

## preparando os dados
# é necessário converter em tensors antes de alimenteas a rede-neural
# Primeiramente criamos um dicionário e representamos cada das 10k palavras mais comuns por um número inteiro
# Nesse caso cada review será representado por uma seuqenia de inteiros
# podemos representar de duas maneiras.
# 1. One-hot-encode os arrays para converte-los em vetores de 0 e 1.
# 2. podemos preencher os arrays para todos terem o mesmo tamanho, então criamos o inteiro tenso de forma

# usando o segundo metodo
num_palavas <- 10000
max_tamanho <- 50
text_vetorizacao <- layer_text_vectorization(
  max_tokens = num_palavas,
  output_sequence_length = max_tamanho
)

# adaptando a camada de vetorização do textto
# nessa parte a camada vai aprender as palavras unicas e assossiar
# um valor inteiro para cada
text_vetorizacao <- text_vetorizacao %>% 
  adapt(df$text)
# vizualização a camada de vetorização
get_vocabulary(text_vetorizacao)
# vizualizando uma aplicação
text_vetorizacao(matrix(df$text[1], ncol = 1))

## construindo um modelo
# decisões sobre a arquitetura
# 1. quantas camadas usar no modelo?
# 2. quantas hiddens units serão usadas em cada camada

entrada <- layer_input( shape = c(1), dtype = 'string')
saida <- entrada %>% 
  text_vetorizacao() %>% 
  # primeira camada: aprendendo o index das palavras
  layer_embedding(input_dim = num_palavas+1, output_dim = 16) %>% 
  # segunda camada: retorna o tamanho fixo. permitindo o modelo a entrada do 'tamanho da variavel'
  layer_global_average_pooling_1d() %>% 
  # terceiro passo: canalização do vetot 'tamanho fixo' por meio de uma camada com 19 unidade escondidas.
  layer_dense(units = 16, activation = 'relu') %>% 
  layer_dropout(0.5) %>% 
  # quarta camada (saida):densamente conectada com um unico nó de saida, utilizando a função de ativação 'sigmoid'
  layer_dense(units = 1, activation = 'sigmoid')

modelo <- keras_model(entrada, saida)

# OBS: Unidades escondidas
# nesse exemplo existe duas camadas intermediárias (entre a de entrada e de saida)
# o número de saudas (units, neurionio, nó) representa a dimensão de espaço da camada.
# ou seja o espaço de aprendizado da camada.
# quanto mais unidades/camdas maior o risco de 'superajuste'

## Função de perda e optimizador
# função que mede a distancia do valor estimado para o subspaço
# configurando o modelo para usar o optmizador e a função de perda
modelo <- modelo %>% compile(
  optimizer= 'adam',
  loss= 'binary_crossentropy',
  metrics= list('accuracy')
)

## treinamento de modelo

historia <- modelo %>% fit(
  treinamento$text,
  as.numeric(treinamento$tag == 'pos'),
  epochs= 10,
  batch_size= 512,
  validation_split= 0.2,
  verbose= 2
)

# metrificando o modelo
modelo %>% evaluate(teste$text, as.numeric(teste$tag == 'pos'), verbose= 0)

# montando o gráfico de acurácia
plot(historia)

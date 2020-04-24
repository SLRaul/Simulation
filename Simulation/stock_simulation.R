 
library(liqueueR)

# dados de entrada
taxa_reposicao <- 0.1
taxa_chegada_cli <- 4

# Dados de início
tam_estoque = 100 #quantidade estoque
li = 10 #limitante inferior p fazer reposição
ls = 100 #limitante superior 
Q = ls - tam_estoque

# relógio 
relogio <- 0
LEF <- PriorityQueue$new()
x <- 0 #quantidade do pedido

inicializador <- function(){
  relogio <<- 0
  tam_estoque <<- 100
  LEF$push('PedidoCli', -rexp(1, taxa_chegada_cli))
  
}

AgendarRep <- function(){
  tam_estoque <<- tam_estoque + Q
  
} 

PedidoCli <- function(){
  x <<- mean(rnorm(1,mean = 10, sd = 4)) #quantidade do pedido
  if(x > tam_estoque){
    if(tam_estoque > li){
      LEF$push('AgendarRep',-(relogio + rexp(1, taxa_reposicao)) )
      Q <<- ls
      
    }else{ 
      tam_estoque <<- 0
    }
  }
  if(x <= tam_estoque){
    
    if(tam_estoque > li && tam_estoque - x <= li){
      Q <<- ls - (tam_estoque - x)
      LEF$push('AgendarRep',-(relogio + rexp(1, taxa_reposicao)) )
      }#else 
    tam_estoque <<- tam_estoque - x
  }
  LEF$push('PedidoCli', -(relogio + rexp(1,taxa_chegada_cli)) )
}

Reposicao <- function(){
  if(tam_estoque + Q > ls)
    tam_estoque = ls
  if(tam_estoque + Q <= ls)
    tam_estoque = tam_estoque + Q
}

inicializador()
Diario <- matrix(ncol = 3)
while(relogio < 600)
{
  Prox_Relogio <- - LEF$priorities[1]
  Evento <- LEF$pop()
  relogio <- Prox_Relogio
  #Processar evento
  if (Evento == "PedidoCli")
  {
    PedidoCli()
  }
  else if (Evento == "AgendarRep")
  {
    AgendarRep()
  }
  
  #Guardar no diário
  linha <- c()
  linha[1] <- relogio
  linha[2] <- x
  linha[3] <- tam_estoque
  Diario <- rbind(Diario, linha)
}

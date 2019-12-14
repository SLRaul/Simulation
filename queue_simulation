library(liqueueR)


#DADOS
Taxa_Chegada <- 3
Taxa_Atendimento <- 4


#SISTEMA
Relogio <- 0
LEF <- PriorityQueue$new()
Tam_Fila <- 0

#ESTATISTICAS


Inicializar <- function()
{
  Relogio <<- 0
  Tam_Fila <<- 0
  #Agendamento inicial
  LEF$push("Chegada", -rexp(1, Taxa_Chegada))
}

Chegada <- function()
{
  Tam_Fila <<- Tam_Fila + 1
  LEF$push("Chegada", -(Relogio+rexp(1, Taxa_Chegada)))
  if (Tam_Fila == 1)
  {
    LEF$push("Atendimento", -(Relogio+rexp(1, Taxa_Atendimento)))
  }
}

Atendimento <- function()
{
  if (Tam_Fila > 1)
  {
    LEF$push("Atendimento", -(Relogio+rexp(1, Taxa_Atendimento)))
  }
  if (Tam_Fila > 0)
  {
    Tam_Fila <<- Tam_Fila - 1
  }
}


Inicializar()
Diario <- matrix(nrow=0,ncol=3)
while(Relogio < 600)
{
  Prox_Relogio <- - LEF$priorities[1]
  Evento <- LEF$pop()
  Relogio <- Prox_Relogio
  #Processar evento
  if (Evento == "Chegada")
  {
    Chegada()
  }
  else if (Evento == "Atendimento")
  {
    Atendimento()
  }

  #Guardar no diário
  linha <- c()
  linha[1] <- Relogio
  linha[2] <- Evento
  linha[3] <- Tam_Fila
  Diario <- rbind(Diario, linha)
}

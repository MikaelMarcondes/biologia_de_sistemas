library(ggplot2)
library(GGally)
library(sna)
library(network)
library(R.utils)
library(plyr)

#Este trecho inicializa
#a variável principal

state_space_convergence <- matrix(, nrow = 32, ncol = 5)
for(i in 1:32){
  for(j in 1:5){
    if(j == 1){
      state_space_convergence[i, j] <- i-1
    }
    else{
      if(j == 2){
        state_space_convergence[, 2] <- c(0,25,7,27,6,27,7,31,12,8,14,15,14,14,14,15,19,19,23,19,23,19,23,23,6,27,7,7,6,26,7,31)
      }
      else{
        state_space_convergence[i, j] <- 0
      }
    }
  }
}

#Este trecho calcula o número de
#arcos entre um estado e seu atrator

for(i in 1:32){
  length <- as.integer(0)
  current_state <- as.integer(i-1)
  next_state <- as.integer(state_space_convergence[current_state + 1, 2])
  while(current_state != next_state){
    length <- length + 1
    current_state <- next_state
    next_state <- state_space_convergence[current_state + 1, 2]
  }
  if(length != 0){
    state_space_convergence[i, 3] <- length
  }
  else{
    state_space_convergence[i, 3] <- as.integer(1)
  }
}

#Este trecho calcula o valor
#a ser atribuído a cada arco

for(i in 1:32){
  counter <- as.integer(0)
  for(j in 1:32){
    if(state_space_convergence[j, 2] == i-1){
      counter <- counter + 1
    }
  }
  if(counter == 0){
    state_space_convergence[i, 4] <- 1
  }
}
state_space_convergence[1, 4] <- 1
check <- as.integer(0)
flag <- as.integer(0)
while(check == 0){
  
  for(i in 1:32){
    
    #Define 'estados de interesse' como aqueles para os quais ainda
    #não há valor atribuído flechas que têm origem nos mesmos
    
    if(state_space_convergence[i, 4] == 0){
      
      #Sinaliza que um 'estado de interesse' foi encontrado
      
      flag <- as.integer(1)
      
      #Separa 'estados de interesse' entre atratores ou não
      
      #Rotina para estados não-atratores
      
      if(state_space_convergence[i, 2] != state_space_convergence[i, 1]){
        for(j in 1:32){
          
          #Busca dentre os destinos de flechas o 'estado de interesse'
          
          if(state_space_convergence[j, 2] == i-1){
            
            #Proíbe o resto do programa de calcular o valor da flecha
            #com origem no 'estado de interesse' se alguma das flechas
            #que têm esse estado por destino ainda não tiverem valores
            #atribuídos por meio de 'dessinalização' do sinalizador.
            
            if(state_space_convergence[state_space_convergence[j, 1] + 1, 4] == 0){
              flag <- 0
            }
          }
        }
      }
      
      #Rotina para estados atratores
      
      else{
        for(j in 1:32){
          
          #Busca dentre os destinos de flechas o 'estado de interesse'
          
          if(state_space_convergence[j, 2] == i-1 && state_space_convergence[j, 1] != state_space_convergence[j, 2]){
            
            #Proíbe o resto do programa de calcular o valor da flecha
            #com origem no 'estado de interesse' se alguma das flechas
            #que têm esse estado por destino ainda não tiverem valores
            #atribuídos (exceção feita à auto-flecha do atrator) por
            #meio de 'dessinalização' do sinalizador.
            
            if(state_space_convergence[state_space_convergence[j, 1] + 1, 4] == 0){
              flag <- 0
            }
          }
        }
      }
    }
    
    #Calcula o valor das flechas de 'estados de interesse' calculáveis
    
    if(flag == 1){
      
      #Rotina para estados não-atratores
      
      if(state_space_convergence[i, 1] != state_space_convergence[i, 2]){
        for(j in 1:32){
          if(state_space_convergence[j, 2] == i-1){
            state_space_convergence[i, 4] <- state_space_convergence[i, 4] + state_space_convergence[state_space_convergence[j, 1] + 1, 4]
          }
        }
      }
      
      #Rotina para estados atratores
      
      else{
        for(j in 1:32){
          if(state_space_convergence[j, 2] == i-1 && state_space_convergence[j, 2] != state_space_convergence[j, 1]){
            state_space_convergence[i, 4] <- state_space_convergence[i, 4] + state_space_convergence[state_space_convergence[j, 1] + 1, 4]
          }
        }
      }
    }
    
    #Desativa o sinalizador para
    #evitar recálculos espúrios
    
    flag <- as.integer(0)
  }
  
  #Verifica se há flechas a serem calculadas
  
  check <- as.integer(1)
  for(i in 1:32){
    check <- check * state_space_convergence[i, 4]
  }
  
}

#Este trecho calcula os 
#w's de cada estado

for(i in 1:32){
  sum <- as.integer(0)
  current_state <- state_space_convergence[i, 1]
  next_state <- state_space_convergence[i, 2]
  if(next_state != current_state){
    while(next_state != current_state){
      sum <- sum + state_space_convergence[current_state + 1, 4]
      current_state <- next_state
      next_state <- state_space_convergence[current_state + 1, 2]
    }
    state_space_convergence[i, 5] <- round(sum/state_space_convergence[i, 3], digits = 2)
  }
  else{
    state_space_convergence[i, 5] <- round(state_space_convergence[i, 4], digits = 2)
  }
}

print(state_space_convergence)
print(state_space_convergence[, 1])
print(mean(state_space_convergence[, 5]))

library(ggplot2)
library(GGally)
library(sna)
library(network)
library(R.utils)
library(plyr)

#Este trecho preenche 'manualmente' as
#entradas da matriz de interações

perceptron_matrix <- matrix(, nrow = 5, ncol = 5)
for(i in 1:5){
  for(j in 1:5){
    perceptron_matrix[i, j] <- 0
  }
}
perceptron_matrix[1, 2] <- 1
perceptron_matrix[1, 5] <- 1
perceptron_matrix[2, 5] <- 1
perceptron_matrix[3, 4] <- 1
perceptron_matrix[4, 3] <- 1
perceptron_matrix[5, 4] <- 1
perceptron_matrix[3, 1] <- -1
perceptron_matrix[4, 1] <- -1

#Encontra a 'cabeça' e a 'cauda' das
#flechas no espaço de estados

transitions <- matrix(, nrow = 32, ncol = 2)
for(i in 0:31){
  
  transitions[i+1, 1] <- as.integer(i)
  
  #Declara e inicia o vetor 'perceptron_state'
  
  perceptron_state <- vector(, 5)
  b <- as.integer(intToBin(i))
  for(j in 1:5){
    #quociente da divisão
    perceptron_state[j] <- b %/% (10 ^ (5 - j))
    #resto da divisão
    if(i != 5){b <- b %% (10 ^ (5 - j))}
  }
  
  #Declara e inicia o vetor 'next_state'
  
  next_state <- vector(, 5)
  for(j in 1:5){
    next_state[j] <- as.integer(0)
  }
  
  #Calcula 'next_state' a partir de
  #'perceptron_state' e 'perceptron_matrix'
  
  for(j in 1:5){
    for(k in 1:5){
      next_state[j] <- next_state[j] + perceptron_matrix[j, k] * perceptron_state[k]
    }
  }
  for(j in 1:5){
    if(next_state[j] > 0){
      next_state[j] <- 1
    }
    else{
      if(next_state[j] < 0){
        next_state[j] <- 0
      }
      else{
        next_state[j] <- perceptron_state[j]
      }
    }
  }
  
  #Calcula a numeração do próximo estado
  
  c <- as.integer(0)
  for(j in 1:5){
    c <- c + 2^(5 - j) * next_state[j]
  }
  transitions[i+1, 2] <- as.integer(c)
  
}

#Calcula a mtriz de adjacências
#do espaço de estados

adj <- matrix(, nrow = 32, ncol = 32)
for(i in 1:32){
  for(j in 1:32){adj[i, j] <- 0}
}
for(i in 1:32){
  adj[transitions[i, 1]+1, transitions[i, 2]+1] <- 1
}

ggnet2(network(adj, directed = TRUE),
       label = 0:31, arrow.size = 8,
       arrow.gap = 0.025)

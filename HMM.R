# Autor: Luiz de Oliveira Alves Filho
# Disciplina: Probabilidade e Processos Estocásticos
# Professor: Mêuser

retorna_b_pelo_indice_linha = function(B,indice_b,indice_linha_b){
  j_linha = indice_linha_b
  j_coluna = 1
  i=1
  while( i <=indice_b){
    b = B[j_linha,j_coluna]
    if(j_linha %% nrow(B) ==0){
      j_linha = 1
      if(ncol(B) >= j_coluna + 1){
        j_coluna = j_coluna + 1 
      }
    }else{
      j_linha = j_linha + 1
    }
    i = i +1
  }
  return(b)
}
retorna_a_pelo_indice_coluna = function(A,indice_a){
  indice_a = indice_a + 1  #por comecar do indice 2
  if(indice_a%%ncol(A)==0){
    return(A[,ncol(A)])
  }else{
    return(A[,indice_a%%ncol(A)])
  }
}
retorna_a_pelo_indice_pela_linha = function(A,indice_a){
  if(indice_a%%nrow(A)==0){
    return(A[nrow(A),])
  }else{
    return(A[indice_a%%nrow(A),])
  }
}

hmm_forward = function(A,B,pi,iteracoes){
  # Inicialização (Equação 6)
  a1 = B[,1] * pi
  alphas = data.frame()
  a2_1 = sum((a1*retorna_a_pelo_indice_coluna(A,1))) * retorna_b_pelo_indice_linha(B,1,2)
  a2_2 = sum((a1*retorna_a_pelo_indice_coluna(A,2))) * retorna_b_pelo_indice_linha(B,2,2) 
  alphas = rbind(data.frame(a2_1,a2_1,a2_2))
  colnames(alphas) = c("alpha1","alpha2","alpha3")
  #   print(alphas)
  
  # Indução (Equação 7)
  aux_alphas =  data.frame(1)
  j = 1
  for(i in ncol(A):iteracoes){
    aux_alphas=  cbind(aux_alphas,sum(alphas[nrow(alphas),] *retorna_a_pelo_indice_coluna(A,i)) * retorna_b_pelo_indice_linha(B,i,2))# OK 
    if(i%%ncol(A) ==0){
      aux_alphas = data.frame(aux_alphas[,-1])
    }
    if(ncol(aux_alphas)==ncol(A)){
      alphas = rbind(data.frame( aux_alphas[,1], aux_alphas[,2], aux_alphas[,3]))
      #       print(alphas)
      colnames(alphas) = c("alpha1","alpha2","alpha3")
      aux_alphas =  data.frame(1)
    }
  }
  
  ################### 2. Cálculo de P(O | lambda ) ####################
  # Terminação (Equação 8)
  print(alphas)
  return(sum(alphas))
}


hmm_backward = function(A,B,pi,iteracoes){
  partes_b = iteracoes/ncol(B)
  indice_coluna_b = ncol(B)
  b_indices_colunas = c()
  for(i in 1: iteracoes){
    b_indices_colunas[i] = indice_coluna_b 
    if(i%%partes_b==0){
      indice_coluna_b = indice_coluna_b - 1
    }
  }
  b_indices_colunas
  betas =  data.frame(1,1,1)
  aux_betas = data.frame()
  for(i in 1:iteracoes){
    aux_betas = rbind(aux_betas , retorna_a_pelo_indice_pela_linha(A,i) * B[,b_indices_colunas[i]] * betas[nrow(betas),])
    if(nrow(aux_betas)%%ncol(A)==0){
      betas = rbind(betas,apply(aux_betas,MARGIN=1,FUN=sum))
      aux_betas = data.frame()
      colnames(betas) = c("beta1","beta2","beta3")
    }
  }
  for(i in 1:nrow(betas)){
    if(betas$beta3[i]==0){
      print(betas[i,])
      return(sum(pi*betas[i,]*B[,1]))      
    }
  }
  #   print(betas[nrow(betas)-1,])
  #   sum(pi*betas[nrow(betas)-1,]*B[,1])
}


viterbi = function(A,B,pi,iteracoes){
  # Inicialização (Equações 15a e 15b)
  delta_atual = pi*B[,1]
  psi = data.frame()
  
  aux_delta =  data.frame(1)
  aux_psi =  data.frame(1)
  delta = data.frame(delta1 = delta_atual[1,1],delta2 = delta_atual[2,1],delta3 = delta_atual[3,1])
  estados = which.max(delta_atual) # definicao do primeiro estado
  for(i in 1:iteracoes){
    a = retorna_a_pelo_indice_coluna(A,i-1)
    b = retorna_b_pelo_indice_linha(B,i,1)
    
    aux_delta = cbind(aux_delta,max(delta_atual*a)*b)
    aux_psi = cbind(aux_psi, which.max(delta_atual*a))
    
    if(i%%ncol(A) ==0){
      aux_delta = data.frame(aux_delta[,-1])
      aux_psi = data.frame(aux_psi[,-1])
      delta_atual = data.frame( delta1 = aux_delta[,1], delta2 = aux_delta[,2], delta3 = aux_delta[,3])
      colnames(delta_atual) = c("delta1","delta2","delta3")
      delta = rbind(delta,delta_atual)
      psi = rbind(psi,data.frame( psi1 = aux_psi[,1], psi2=aux_psi[,2], psi3= aux_psi[,3]))
      colnames(delta) = c("delta1","delta2","delta3")
      colnames(psi) = c("psi1","psi2","psi3")
      aux_psi =  data.frame(1)
      aux_delta =  data.frame(1)
    }
  }
  estados = c(estados,psi[-1,3]) # a forma de definir o primeiro estado eh diferente da recursao
#   print(delta)
#   print(psi)
  
  print("Backtracking")
  print(estados)
  #   Terminação (Equações 17a e 17b)
  maximo = max(delta[nrow(delta),])
  indice = which.max(delta[nrow(delta),])
  return(data.frame(maximo = maximo,indice = indice))
}

A = matrix(c(.3,0,0,.5,.3,0,.2,.7,1),nrow=3,ncol=3)
B = matrix(c(1,.5,0,0,.5,1),nrow=3,ncol=2)
pi = matrix(c(.6,.4,0),nrow=3,ncol=1)

hmm_forward(A,B,pi,8)
hmm_backward(A,B,pi,12)
viterbi(A,B,pi,12)









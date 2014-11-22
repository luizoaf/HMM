retorna_b_pelo_indice_linha = function(B,indice_b){
  j_linha = 2
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

A = matrix(c(.3,0,0,.5,.3,0,.2,.7,1),nrow=3,ncol=3)
B = matrix(c(1,.5,0,0,.5,1),nrow=3,ncol=2)
pi = matrix(c(.6,.4,0),nrow=3,ncol=1)

#inicializacao
a1 = B[,1] * pi
a1
alphas = data.frame()
i = 1
a2_1 = sum((a1*retorna_a_pelo_indice_coluna(A,i))) * retorna_b_pelo_indice_linha(B,i)# OK 
i = 2
a2_2 = sum((a1*retorna_a_pelo_indice_coluna(A,i))) * retorna_b_pelo_indice_linha(B,i)# OK 
alphas = rbind(data.frame(a2_1,a2_1,a2_2))
colnames(alphas) = c("alpha1","alpha2","alpha3")

aux_alphas =  data.frame(1)
j = 1
for(i in 3:8){
  aux_alphas=  cbind(aux_alphas,sum(alphas[nrow(alphas),] *retorna_a_pelo_indice_coluna(A,i)) * retorna_b_pelo_indice_linha(B,i))# OK 
  if(i%%3 ==0){
    aux_alphas = data.frame(aux_alphas[,-1])
  }
  if(ncol(aux_alphas)==3){
    alphas = rbind(data.frame( aux_alphas[,1], aux_alphas[,2], aux_alphas[,3]))
    colnames(alphas) = c("alpha1","alpha2","alpha3")
    aux_alphas =  data.frame(1)
  }
}


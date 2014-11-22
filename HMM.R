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

i = 1
sum((a1*retorna_a_pelo_indice_coluna(A,i))) * retorna_b_pelo_indice_linha(B,i)# OK 
i=2
sum((a1*retorna_a_pelo_indice_coluna(A,i))) * retorna_b_pelo_indice_linha(B,i)# OK 
if(i )
  i=3
sum((a1*retorna_a_pelo_indice_coluna(A,i))) * retorna_b_pelo_indice_linha(B,i)# OK 



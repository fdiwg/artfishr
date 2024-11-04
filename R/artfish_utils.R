
#'@name artfish_accuracy
#'@title artfish_accuracy
#'@param n n
#'@param N N
#'@param method method
#'@export
artfish_accuracy<-function(n,N,method="higher"){
  #Algebraic approach
  NP<-function(n,N){
    W=0.75*(1-1/N)
    a=(2*W*(N^2))/(N-1)^2-(N+1)/(N-1)
    g=a+(1-a)/N
    S=(1-a)*(1/log(N)-1/(N*log(N))-1/N)
    k=(-2/log(N))*log(S/(1-S-g))
    a2=(1-S-g)^2/(2*S+g-1)
    a1=g-a2
    x=log(n)/log(N)
    A=a1+a2*(N^(-k*x))
    
    return(A)
  }
  
  #Probabilistic approach
  P<-function(n,N){
    R=sqrt((2*N-1)/(6*(N-1))-1/4)
    A=1-1.96*(R/sqrt(n))*sqrt(1-n/N)
    
    return(A)
  }
  
  Acc<-switch(method,
              "algebraic"=NP(n=n,N),
              "probabilistic"=P(n,N),
              "higher"=max(NP(n,N),P(n,N)))
  return(Acc)
}

#'@name unif_index
#'@title unif_index
#'@param days days
#'@export
unif_index<-function(days){
  table<-as.data.frame(table(days))
  mean=mean(table$Freq)
  table$ratio<-ifelse(table$Freq/mean>1,1,table$Freq/mean)
  index=mean(table$ratio)
  return(index)
}


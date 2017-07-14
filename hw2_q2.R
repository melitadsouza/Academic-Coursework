a = 1;
val_rndm=matrix(NA, nrow = 50, ncol = 3)
repeat{
  X=matrix(NA, nrow=1000, ncol=a)
  for(i in 1:a)
    X[,i]=c(rnorm(1000, mean = 0, sd=1))
  
  Z=matrix(NA, nrow=1000, ncol=1000)    
  for(i in 1:1000){
    for(j in 1:1000){
      Z[i,j]=sum(X[i,]*X[j,])/sqrt(sum(X[i,]*X[i,])*sum(X[j,]*X[j,]))
    }
  }
  val_rndm[a,]=c(a, mean(Z), sd(Z));
  
  a=a+1
  if(a==51){
    break;
  }
}

plot(val_rndm[,1], val_rndm[,2])
plot(val_rndm[,1], val_rndm[,3])
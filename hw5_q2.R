#referred to weak_voting.r
# demonstration of what happens when we vote using a collection of indpendent classifiers, only slighly better
# than random guessing.  


M = 100;	  	# number of indepenent classifiers  (odd to avoid tie)
N = 1000;       # trials

C = rep(0,M)
error = 0
for (n in 1:N) {
  trueclass = sample(c(0,1),1); 
  if (trueclass == 1) { p = c(.45,.55); }
  else { p = c(.55,0.45); }
  C = sample(c(0,1),M,replace=T,prob=p);
  est = ifelse (sum(C) > M/2,1,0); 
  if (est != trueclass) error = error+1
}
print("error rate:")
print(error/N)

C = rep(0,M)
#class =C(0,1,2)
error = 0
for (n in 1:N) {
  trueclass = sample(c(0,1,2),1);
  if (trueclass == 0) { 
    p = c((1/3)+0.1, (1/3)-0.005, (1/3)-0.005); }
  else if (trueclass == 1) { 
    p = c((1/3)-0.005, (1/3)+0.1, (1/3)-0.005); }
  else { 
    p= c((1/3)-0.005, (1/3)-0.005, (1/3)+0.1);}
  C = sample(c(0,1,2),M,replace=T,prob=p);
  sum0=length(C[which(C==0)])
  sum1= sum(C[which(C==1)])
  sum2= sum(C[which(C==2)])/2
  
  if(sum0 >= sum1 & sum0>= sum2) 
  {
    est = 0
  } else if(sum1> sum2 & sum1> sum0){
    est=1
  } else {
    est=2
  }
  #print(est)
  #print(trueclass)
  if (est != trueclass) error = error+1
}
print("error rate:")
print(error/N)
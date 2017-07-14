#Name:Melita Dsouza(dsouzam@iu.edu)
#prob matrix of A,B,C,D
abcd=matrix(c(0,1,0,0,0,0,0.5,0.5,0.5,0,0,0.5,1,0,0,0),nrow=4,ncol=4)
transpose=t(abcd)

#all zero matrix of 10 rows and 4 cols
prob=matrix(0,nrow=10,ncol=4)
#prob of X1=A=1
prob[1,1]=1

i=2
while(i<=10)
{
  j=1
  while(j<=4)
  {
    k=1
    while(k<=4)
    {
      prob[i,j] = prob[i,j] + prob[i-1,k]*transpose[k,j]
      k=k+1
    }
    j=j+1
  }
  i=i+1
}
print(prob)

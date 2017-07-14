#referred to quadratic_svm.r
#install.packages("quadprog")
library("quadprog")
#set.seed(350)
#creating 50 2-d data points
x1 <- runif(50,min = -pi, max = +pi)
x2 <- runif(50,min = -pi, max = +pi)
data<-data.frame(x1,x2)

# the vector of {-1,1} elements giving classes 
y <- rep(0,50)

#creating a weight vector with N(0,1)
w <- rnorm(5,mean=0,sd=1)

for(i in 1:50){
  if(w[1] + w[2]*x1[i] + w[3]*x2[i] + w[4]*cos(x1[i]) + w[5]*sin(x1[i]) > 0){
    y[i]=1
  }
  else{
    y[i]=-1
  }
}

#plot x1 and x2
#Q_1a
plot(x1,x2,pch=y+2,col=y+3,main="Plot of two classes",cex=1)

X <- cbind(x1,x2,cos(x1),sin(x1),rep(1,50))

N=50
feat = 5;
# construct the corresponding elements: D,d, A, b
dim = feat + N;

D = diag(1,dim)

D[feat,feat] = 0.001
d=rep(0,dim)
A = matrix(0,2*N,dim)
A[1:N,1:5 ] = X*y
A[1:N,(feat+1):dim] = diag(N)
A[(N+1):(2*N),(feat+1):dim   ]  = diag(N)
b = rep(0,2*N)
b[1:N] = 1

result = solve.QP(D,d,t(A),b)
what = result$solution[1:feat]


xa = seq(-pi,pi,by=.01);
#solving for x2 given the linear equation w0+w1x1+w2x2+w3cos(x1)+w4sin(x1)
xb  = -(( what[1]*xa + what[3]*cos(xa) + what[4]*sin(xa) + what[5])/what[2])
lines(xa,xb)



#referred to https://qizeresearch.wordpress.com/2013/12/05/short-example-for-adaboost/
#referred to http://mccormickml.com/2013/12/13/adaboost-tutorial/
library("rpart")
x <- rnorm(1000)#x~N(0,1)
y <- rep(0,1000)#creating a vector of 1000 zeros

#mod of xi lies between -1 and +1
for(i in 1:1000)
  {
  if(x[i]>-1&&x[i]<1)
    {
    if(runif(1)<=0.9)
      {y[i]<- 1}
    else{y[i]<- -1}
  }
  else
    {
    if(runif(1)<=0.9){y[i]<- -1}
    else{
      y[i]<- -1
    }
    
}
} 
#step 1:generating weights:
w <- rep(1/1000,1000)
adadata<-data.frame(x,y)
#creating variable for output
output <- 0
#creating base classifier h
h <- rep(0,1000)
#step2:looping from t=1 to 100 i.e 100 iterations
for (t in 1:100){
  
  #Sub-step2.1: select a base classifier "h" (can be rpart), and train data with the given weight
  o <- rpart(y ~ ., data = adadata, weights =  w , method = "class")
  
  h <- as.matrix(predict(o,adadata,type = "class"))
  
  #setting class h as a numeric argument
  class(h)<-"numeric" 
  
  #Sub-step2.2: Typically, we recognize the err of the classifier: Err(t)=sum(D(i)*I(yi^=ht((xi))). In other word, we sum D(i) for all the observations with the predicted value not equal to the true value.
  error = sum(w*(h!=y))
  
  #creating o/p weight alpha for the classifier using formula a(t)=1/2ln(1-err/err)
  alpha = 0.5*log((1-error)/error)
  
  #Sub-step2.3: update Weight Wi(t+1) = Di(t)*exp(-a(t)*Yi*ht(Xi))
  w <- w*exp(-alpha*y*h)/sum(exp(-alpha*y*h))
  #Sub-step2.4:normalized weight for each observation: Di(t+1)=Wi(t+1)/Sum(Wi(t+1)). In other word, Di(t+1) is normalized Wi(t+1) in t+t step.
  w <- w/sum(w)
  
  #final: step 3: H(x)=sign(sum(a(t)*h)
  output <- output + alpha*h
}
#returning a vector with the signs of corresponding elements of ouput
output <- sign(output)
table(actual = y, predicted = output)

plot(x,output,col = output+5)

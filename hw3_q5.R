n = 1000;
x = rep(0,n);
s = c(1,5,4,2,3);
k = length(s);
for (i in (k+1):n) {
  j = s[(i %% k) + 1]; # i %% k is i mod k
  x[i] = 1 - x[i-j];
}


library(rpart)

x1 = x2 = x3 = x4 = x5 <- rep(0,n) # intilialize variables

dim = length(x)
# creating xi as a function of past m values
x1[1:(dim-1)] <- x[2: dim]
x2[1:(dim-2)] <- x[3: dim]
x3[1:(dim-3)] <- x[4: dim]
x4[1:(dim-4)] <- x[5:dim]
x5[1:(dim-5)] <- x[6:dim]
pred <- data.frame(x1,x2,x3,x4,x5)

tree = rpart(x~ ., data = pred , method = "class") # predict x from x1,x2,x3,x4,x5
plot(tree, uniform=TRUE)
text(tree, use.n=TRUE, all=TRUE,cex=0.6)

 #classification tree
pred_rows <- nrow(pred) 

predict = predict(tree ,pred[1:pred_rows,],type="class") # predict m
# confusion matrix
table(real = x, estimated = predict)

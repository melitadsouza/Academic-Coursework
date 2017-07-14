#Name:Melita Dsouza(dsouzam@iu.edu)
#referred to gaussian_classifier.r

data=read.table("../glass.data",sep=",")
dim(data)
data = subset(data, V11== 1 | V11==2 | V11==7) #only clasess 1,2,7
index=sample(nrow(data),0.5*nrow(data))
print(index)
train=data[index,]
dim(train)
test=data[-index,]
dim(test)

#hw4_q4b
n=nrow(train)
type = rep(0,n)
type[train[,11] == "1"] = 1; 
type[train[,11] == "2"] = 2;
type[train[,11] == "7"] = 7;


X1 = train[type==1,2:10];
X2 = train[type==2,2:10];
X3 = train[type==7,2:10];

m1 = colMeans(X1);
m2 = colMeans(X2);
m3 = colMeans(X3);


S1 = solve(cov(X1));	
S2 = solve(cov(X2));
S3 = solve(cov(X3));

det1 = 1/det(S1);
det2 = 1/det(S2);
det3 = 1/det(S3);

d = rep(0,3);
c = rep(0,n)
for (i in 1:n) {
  x = train[i,2:10];
  d[1] =  log(det1) + sum((x-m1) * S1 %*% t(x-m1))
  d[2] =  log(det2) + sum((x-m2) * S2 %*% t(x-m2))
  d[3] =  log(det3) + sum((x-m3) * S3 %*% t(x-m3))
  c[i] = which.min(d);
}
train_error_rate = sum(c != type)/n
print(train_error_rate)

#hw4_q4c
n=nrow(test)
type = rep(0,n)
type[test[,11] == "1"] = 1; 
type[test[,11] == "2"] = 2;
type[test[,11] == "7"] = 7;


X1 = test[type==1,2:10];
X2 = test[type==2,2:10];
X3 = test[type==7,2:10];

m1 = colMeans(X1);
m2 = colMeans(X2);
m3 = colMeans(X3);


S1 = solve(cov(X1));	
S2 = solve(cov(X2));
S3 = solve(cov(X3));

det1 = 1/det(S1);
det2 = 1/det(S2);
det3 = 1/det(S3);

d = rep(0,3);
c = rep(0,n)
for (i in 1:n) {
  x = test[i,2:10];
  d[1] =  log(det1) + sum((x-m1) * S1 %*% t(x-m1))
  d[2] =  log(det2) + sum((x-m2) * S2 %*% t(x-m2))
  d[3] =  log(det3) + sum((x-m3) * S3 %*% t(x-m3))
  c[i] = which.min(d);
}
test_error_rate = sum(c != type)/n
print(test_error_rate)

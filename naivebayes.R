#Name:Melita Dsouza(dsouzam@iu.edu)
# read the data
data=read.table("../naive_bayes_binary.csv",sep=",",header=TRUE)

# split into training and test data
train = data[1:2500, ];
test = data[2501:5000, ];

# number of classes
count = 3 # class count

# splitting into classes
class1 = subset(train, V11 == 1, select=c(-V11));
class2 = subset(train, V11 == 2, select=c(-V11));
class3 = subset(train, V11 == 3, select=c(-V11));

# class probabilities
class_prob = matrix(0, nrow=count, ncol=10)
class_prob[1, ] = colSums(class1)/nrow(class1)
class_prob[2, ] = colSums(class2)/nrow(class2)
class_prob[3, ] = colSums(class3)/nrow(class3)

# conditional probabilities
new_class = rep(0,2500)
prev = rep(0.33,3)

# loop through each record in test set 
for(i in 1:2500) {
  rec = test[i,]
  curr=prev
  # cond probability of each variable
  for (j in 1:count) {
    for (k in 1:10){
      if( rec[k]==1) {
        prob = class_prob[j,k]
      } else {
        prob=1-class_prob[j,k]
      }
      curr[j] = curr[j]*prob
    } 
  } 
  new_class[i] = which.max(curr)
}

old_class <- test[,11]
# confusion matrix
table(new_class,old_class)

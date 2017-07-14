#reference:http://www.statmethods.net/advstats/cart.html

mathdata=read.table("../student-mat.csv",sep=";",stringsAsFactors=FALSE,header=TRUE)

mathdata$G3[mathdata$G3 > 10] = 1
mathdata$G3[mathdata$G3 != 1] = 0

library(rpart)
d_tree=rpart(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
  + Fjob + reason + nursery + internet + guardian + traveltime + studytime
  + failures + schoolsup + famsup + paid + activities + higher + romantic
  + famrel + freetime + goout + Dalc + Walc + health + absences,method="class",data=mathdata)

d_tree=rpart(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + Fjob + reason + nursery + internet + guardian + traveltime + studytime
             + failures + schoolsup + famsup + paid + activities + higher + romantic
             + famrel + freetime + goout + Dalc + Walc + health + absences, 
             data = mathdata, method ="class",parms = list(split = 'gini'))

plot(d_tree, uniform=TRUE, 
     main="Classification Tree")
text(d_tree, use.n=TRUE, all=TRUE,cex=.7)

pruned_tree=prune(d_tree,cp=0.03225806)
printcp(pruned_tree)
summary(pruned_tree)
plot(pruned_tree, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pruned_tree, use.n=TRUE, all=TRUE,cex=.7)


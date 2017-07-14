# simple implementation of k-means clustering.  In this implementation the number of eventual
# cluster may be less than the number we start with.  That is, if a particular centroid
# is not closest to any of the points, the centroid will disappear




K = 4	# number of clusters
n = 50  # points in each cluster
D = 2;	# dimension of vectors (small for visualization's sake)

N = K*n;  # total number of points
b = array(10*rnorm(K*D),c(K,D));  # cent[k,] is the center for the kth cluster
T = array(rnorm(K*D*D),c(K,D,D));    # T[k,,] is the linear trans applied to get the kth cluster
X = matrix(0,nrow=D,ncol=0);
for (k in 1:K) {
  m = b[k,];
  L = T[k,,];
  for (i in 1:n) X = cbind(X,L %*% rnorm(D)+m);
}
X = t(X);
plot(X[,1],X[,2]);



proto = matrix(rnorm(K*D),K,D);   # proto[k,] will be center (prototype) of kty cluster
dist = matrix(0,K,N); 		  # dist[k,i] is distance from ith point to kth cluster center
cluster = rep(0,N);		  # cluster[i] is cluster currently asigned to X[i,]

for (iter in 1:10) {
  for (i in 1:N) {
    for (k in 1:K) {  # compute distance matrix 
      dist[k,i] = (X[i,1]-proto[k,1])^2 + (X[i,2]-proto[k,2])^2 
    }	    
    cluster[i] = which.min(dist[,i]);  # assign each point to closest center
  }
  plot(X[,1],X[,2],col=cluster);  # plot using color for the current clusters
  points(proto,pch='x',col=1:K,cex=3);   	 # plot cluster centers as well
  for (k in 1:K) {
    if (sum(cluster == k) == 0) next;
    if (sum(cluster == k) == 1) next;	# don't reestimate center if only have 0-1 examples
    proto[k,] = colMeans(X[cluster==k,]);   # reestimate kth cluster center
  }
  print("enter return to continue");		
  readline();					# wait for keyboard input (so we can see what happened so far)
}


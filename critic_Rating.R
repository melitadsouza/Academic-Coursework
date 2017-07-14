#ratings by reviewers a and b
critic_a=runif(n=1000,min=0,max=6)
critic_b=runif(n=1000,min=4,max=10)
#converting into ecdf
ecdf_a=ecdf(critic_a)
ecdf_b=ecdf(critic_b)
x=ecdf_a(critic_a)*100
y=ecdf_b(critic_b)*100
#scatterplot
plot(x,y)

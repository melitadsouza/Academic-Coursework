n=1000
x=rexp(n)
y=ecdf(x)
plot(y)

z=y(x)
plot(ecdf(z))


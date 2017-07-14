
file = read.csv("4_9.csv")
size 	= file[, 2]
decTree = file[, 3]/100
naiBay = file[, 4]/100
svm = file[, 5]/100
n = nrow(file)

w1 = 0
w2 = 0
w3 = 0
l1 = 0
l2 = 0
l3 = 0
for (i in 1:n) {
	p = (decTree[i] + naiBay[i])/2;
      Z = (decTree[i] - naiBay[i])/(sqrt(2*p*(1-p)/size[i]))
	if(Z > 1.96) {
		w1 = w1 + 1;
	}
	else if(Z < -1.96) {
		l1 = l1 + 1;
	}

	p = (naiBay[i] + svm[i])/2;
      Z = (naiBay[i] - svm[i])/(sqrt(2*p*(1-p)/size[i]))
	if(Z > 1.96) {
		w2 = w2 + 1;
	}
	else if(Z < -1.96) {
		l2 = l2 + 1;
	}

	p = (svm[i] + decTree[i])/2;
      Z = (svm[i] - decTree[i])/(sqrt(2*p*(1-p)/size[i]))
	if(Z > 1.96) {
		w3 = w3 + 1;
	}
	else if(Z < -1.96) {
		l3 = l3 + 1;
	}
}
cat('\ndecTree VS naiBay\n');
print(w1)
print(l1)
print(n-w1-l1)

cat('\nnaiBay VS svm\n');
print(w2)
print(l2)
print(n-w2-l2)

cat('\nsvm VS decTree\n');
print(w3)
print(l3)
print(n-w3-l3)

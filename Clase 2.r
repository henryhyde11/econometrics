X2 = c(32,40,36,44,48)
Y = c(84,108,92,110,106)
X = matrix(c(rep(1,length(X2)), X2), ncol = 2) #matrix X
X

t(X)

txx = t(X)%*%X    #matrix X’X
txx

invtxx = solve(txx)    #inversa de la matrix X’X
invtxx

txy = t(X)%*%Y    #matrix X’y
txy

bt = invtxx%*%(txy)    #matrix b
bt
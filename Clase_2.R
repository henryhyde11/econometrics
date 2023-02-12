## Residual Sum of Squares.

Y = c(84,108,92,110,106)
X2 = c(32,40,36,44,48)
X = matrix(c(rep(1,length(X2)), X2), ncol = 2) #matrix X
X

t(X)    #Transpose X.

txx = t(X)%*%X    #Matrix X’X
txx

invtxx = solve(txx)    #Inverse matrix X’X
invtxx

txy = t(X)%*%Y    #Matrix X’y
txy

bt = invtxx%*%txy
bt


Yest = X%*%bt    #Y estimado
ug = Y - (X%*%bt)    #Vector de residuos
ug

sum(ug)    #Suma de los residuos

t(X)%*%ug # Matriz X’ug

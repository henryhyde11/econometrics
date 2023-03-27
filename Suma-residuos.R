## Residual Sum of Squares.

y = c(84,108,92,110,106)
x2 = c(32,40,36,44,48)
x = matrix(c(rep(1,length(X2)), X2), ncol = 2) #matrix X
x

t(x)    #Transpose X.

txx = t(x)%*%x    #Matrix x’x
tx

invtx = solve(tx)    #Inverse matrix x’x
invtx

txy = t(x)%*%y    #Matrix x’y
txy

bt = invtx%*%txy
bt


yest = x%*%bt    #y estimado
ug = y - (x%*%bt)    #Vector de residuos
ug

sum(ug)    #Suma de los residuos

t(x)%*%ug # Matriz x’ug
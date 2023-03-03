## Modelo en desviaciones.

attach(Datos_Demanda_Un_Bien)

data = Datos_Demanda_Un_Bien
data

y = cbind(data$Y)[1:35,]
X2t = data$X2
X3t = data$X3
X4t =data$X4

"VARIABES EN DESVIACIONES Y MATRICES DE DATOS"
"Definimos las variables en desviaciones a la media"

ya1 = y - mean(y)
x2a = X2t - mean(X2t)
x3a = X3t - mean(X3t)
x4a = X4t - mean(X4t)


"Tabla de medias"

cm1 = c("Variable", "y*", "X2*", "X3*", "X2*" )
cm2 = c("media muestra", mean(ya1), mean(x2a), mean(x3a), mean(x4a))

(T_M = cbind(cm1, cm2))


"Las matrices de datos"
"La matriz X2"
data
X2 = as.matrix(cbind(data[1:35,3:5]))

"Forma alternativa"
X2_1 = as.matrix(cbind(X2t,X3t,X4t))

"La matriz X2a"
X2a1 = as.matrix(cbind(x2a,x3a,x4a))

"Definiciónde la matriz de transformación A"
"La matriz identidad de orden n = 35"

n=35

I35 = diag(n)

"La matriz iit"
"El vector i"
I = cbind(rep(1,n))

"La matriz IIt"
IIt = I%*%t(I)

"Otra opción es"
IIt1 = matrix(nrow = n, ncol = n, 1)

"La matriz de transformación A es"
A = I35-(1/n)*IIt

"La matriz X* y el vector y* son los siguientes:"
"Matriz X* es"
Xa = A%*%X2

"El vector y* es"
ya = A%*%y


#---------------------------------------------


"La matriz X*tX* es"
(XatXa = t(Xa)%*%Xa)

"La matriz X*tX* inversa es"
(XatXa_inv = solve(XatXa))

"El Vector X*ty* es"
(Xatya = t(Xa)%*%ya)

"El estimador b2 es"
b2 = XatXa_inv%*%Xatya
b2

"El intercepto esimado es"
(b1a = mean(y) - b2[1]*mean(X2t) - 
    b2[2]*mean(X3t) - b2[3]*mean(X4t))

"El estimador b de OLS es"
b = rbind(b1a, b2)
b


#---------------------------------------------

"El modelo en desviaciones usando la función lm()"
'Se coloca -1 para no estimar la constate '

x2aa = cbind(x2a)[1:35,]
x3aa = cbind(x3a)[1:35,]
x4aa = cbind(x4a)[1:35,]

eq1a = lm(ya ~ x2aa + x3aa + x4aa - 1)


# En esta ecuación no hay intercepto y se excluye con -1 en la función lm()
seq1a = summary(eq1a)
seq1a


names(eq1a)

coef = eq1a$coefficients
coef

res = eq1a$residuals
res
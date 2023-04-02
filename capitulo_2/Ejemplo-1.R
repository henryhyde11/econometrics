# Ejemplo 1 - Capitulo 2. Prueba de prnósticos de Chow. Ejercicio 4.1 Dinardo. ^2

#ESTADÍSTICA PRONÓSTICOS DE CHOW DE LA ECUACIÓN
#ETAPA 1: ESTIMADOR b CON LA PRIMERA MUESTRA
#El tamaño de la muestra 1

n1 = 20
k = 2
alpha = 0.05

"La matriz X1tX1 es"
(X1tX1 = matrix(nrow = k, ncol = k, c(20,10,10,30)))

"El vector X1ty1 es"
(X1ty1 = cbind(c(30,40)))

"El y1ty1 es"
(y1ty1 = cbind(c(75)))

"----El estimador b1 con la primera muestra--"
"La matriz X1tX1_inv es"
(X1tX1_inv = solve(X1tX1))

"El estimador b1 es"
(b1 = X1tX1_inv%*%X1ty1)



# Sumas de Cuadrados

" ---El TSS1  ---"
TSS1 = (y1ty1 - n1*(30/20)^2)[1]
TSS1

"--- El ESS1 --- "
ESS1 = (t(b1)%*%X1tX1%*%b1 - n1*(30/20)^2)[1]
ESS1

"--   El RSS1 ----"
RSS1 = TSS1 - ESS1
RSS1

"La varianza estimada"
S12 = RSS1/(n1 - k)
S12


#ETAPA 2: MATRIZ X2, y el VECTOR y2 son  

"La MATRIZ X2 ES"
X2 = rbind(c(1,2))
X2

"El vector y2"
y2 = cbind(c(4))
y2


#ETAPA 3: EL y2 PRONOSTICADO ES

"EL VECTOR DE PRONÓSTICO ES"
y2p = X2%*%b1
y2p


#ETAPA 4: EL ERROR DE PRONÓSTICO ES

"Los errores de pronóstico son"
d = y2 - y2p
d


#ETAPA 5: LA ESTADÍSTICA DE PRUEBA DE LA ECUACIÓN (8) ES

"El tamaño de la muestra 2"
n2 = 1

"LA MATRIZ IDENTIDAD DE ORDEN n2 = 1 es"
In2 = diag(n2)
In2

"El valor calculado de la estadística F de la ecuación (8) es"
Fc_8 = ((t(d)%*%solve(In2+X2%*%X1tX1_inv%*%t(X2))%*%d)[1]/n2)/(RSS1/(n1-k))
Fc_8

"Para tomar la decisión buscamos el p - valor"
pv_ec8 = pf(Fc_8, df1 = n2, df2 = n1-k, lower.tail = F)
pv_ec8

if(pv_ec8 >= alpha){
  "Hay Estabilidad de parámetros"
} else {
  "No hay estabilidad de parámetros"
}


# ESTIMACIÓN CON LA MUESTRA COMPLETA

"ESTADÍSTICA DE LA ECUACIÓN 9"
"Estimar el modelo con las n = 21 observaciones"
n = 21

"La matriz XtX"

XtX = matrix(nrow = k, ncol = k, c(21,12,12,34), byrow = F)#lee datos por columnas; para que lea por filas se le da la opcion byrow = t
XtX

"El vector Xty"
Xty = cbind(c(34,48))
Xty

"El vector yty"
yty = cbind(c(91))
yty

"La matriz XtX_inv"
XtX_inv = solve(XtX)
XtX_inv

"El estimador de Beta usando la muestra completa es"
b = XtX_inv%*%Xty
b


"Las sumas de cuadrados del modelo con todas las n = 21 observaciones"
"TSS"

TSS = (yty - 21*(34/21)^2)[1]
TSS

"El ESS"
ESS = (t(b)%*%XtX%*%b - n*(34/21)^2)[1]
ESS

"El RSS"
RSS = TSS - ESS
RSS

"La estadística F calculada de la ecuación (9) es"
Fc_9 = (RSS - RSS1) / (RSS1 / (n1 - k))
Fc_9

"La estadística F de la ecuación (8) es"
Fc_8

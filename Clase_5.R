# Ejemplo 2: con logaritmos.

install.packages("car")
install.packages("readxl")
install.packages("Rcpp")

library(Rcpp)
library(car)
library(readxl)


Datos = read_excel("C:/Users/USUARIO/econometrics/Datos_Demanda_Un_Bien.xls")
View(Datos)

Datos=data.frame(Datos) 
attach(Datos) 
head(Datos)
tail(Datos)


"El valor de k = 4"
k = 4

"El nivel de significancia es alpha = 0.05"
alpha = 0.05

"El tamaño de la muestra es n = 35"
n = 35 

y = log(Datos$Y)
x2 = log(Datos$X2)
x3 = log(Datos$X3)
x4 = log(Datos$X4)

datos = cbind(y, x2, x3, x4)
View(datos)


# "MATRIZ DE DATOS X0 las 38 observaciones"
x0 = as.matrix(cbind(1, datos[,2:4]))
x0

"El vector y0 de longitud 38 con tres observaciones faltantes"
y0 = cbind(y)
y0

"LA MATRIZ X ES"
x = x0[1:35,]

"El vector y es"
y = cbind(datos[1:35, 1])


"El tamaño de la muestra es"
(n=length(y))

#Las variables son:

#La varible X2t
x2t = cbind(datos[,2])

"La varible X3t"
x3t = cbind(X[,3])

"La varible X4t"
x4t = cbind(X[,4])

"LA MATRIZ X'X ES: LA FUNCIÓN t(X) nos da la transpuesta de la matriz X"
(xtx = t(x)%*%x)

"El determinante de la matriz XtX es"
(det_xtx = det(xtx))

"LA MATRIZ X'X inversa ES"
xtx_inv = solve(xtx)
xtx_inv

"EL VECTOR Xty ES"
xty = t(x)%*%y
xty

"EL ESTIMADOR OLS b ES"
b = xtx_inv%*%xty
b

"EL yt estimado es"
ye = cbind(x%*%b)
ye



"Otra alternativa yte1=b1+b2x2t ...."
yte1 = b[1] + (b[2] * X2t) + (b[3] * X3t) + (b[4] * X4t)
yte1


"DIAGRAMA DE DISPERSIÓN Yt VS Yt ESTIMADO" 
"SE USA LA FUNCIÓN plot"

plot(y, ye, type = "p", pch = 8, col="red",
     main = "Diagrama de dispersión\n Yt vs Yte", 
     xlab ="Ventas observadas",
     ylab="Ventas estimadas")

abline(a = 0, b = 1, lty = 2) # La opción gráfica lty da el tipo de línea


"Otra alternativa: Un diagrama lineal con Yt y Yt_e 
en el eje vertical"

plot(y, type = "l", main = "Comparación Yt vs Yt_e", ylab = "Yt - Yt_e", xlab = 
       "Indice", lty = 1, lwd = 1, ylim = c(35,125))

lines(ye, col="red", lty = 2, lwd = 1)
legend(x=1, y=122, legend = c("Observado","Estimado"),
       lty = c(1,2), lwd = c(1,1))


"La matriz de pronósticos es "
xf = x0[36:38,]
xf

"Los pronósticos para los años 2006 al 2008 son"
yf = cbind(xf%*%b)
yf

"Tabla de pronósticos"
c1 = c("Año", 2006, 2007, 2008)
c2 = c("Pronóstico", round(yf[1], 4), round(yf[2],4), round(yf[3],4))
T_P = cbind(c1,c2)
T_P


"Otra alternativa: Un diagrama lineal con Yt y Yt_e en el eje vertical"

plot(y, type = "l", main = "Comparación Yt vs Yt_e",
     ylab = "Yt - Yt_e",
     xlab = "Indice", lty = 1, lwd = 1, ylim = c(35,125))

lines(ye, col="red", lty = 2, lwd = 1)
legend(x=1, y=122, legend = c("Observado","Estimado"), lty = c(1,2), lwd = c(1,1))



"El vector y0 estimado es:"
y0e = X0%*%b


"Gráfica de los pronósticos"

plot(y0e, type = "l", main = "Grafica de los pronósticos",
     ylab = "Y0te - Yt0",
     xlab = "Indice",
     lty = 1, lwd = 1, ylim = c(40, 135))

lines(y0, col="red", lty = 2, lwd = 1)

legend(x=1, y=128, legend = c("Estimado","Observado"), lty = c(1,2), lwd = c(1,1), 
       col=c(1,2))




"EL VECTOR DE ERRORES DE ESTIMACIÓN ES"
e = y - ye

"Las estadísticas descriptivas del vector e son"
summary(e)

"Verificación de que la media de los errores de estimación es 0"
me = round(mean(e), 4)
me

"Verificación de X'e = 0"
"El vector X'e es"
(Xte=round(cbind(t(X)%*%e),4))




"PRUEBA DE NORMALIDAD"
"HISTOGRAMA DE RESIDUALES: SUPERPONEMOS LA DISTRIBUCIÓN 
NORMAL CON MEDIA 0 Y CON LA DESVIACIÓN ESTÁNDAR DE e"

hist(e, seq(-6, 8, by=2) , prob=TRUE, main = "Histograma de residuales",
     xlab = "Errores de estimación",
     ylab = "Freq. Relativa", xlim = c(-7,9))

curve(dnorm(x, mean = me, sd = sd(e)),
      col="red",lty=2,lwd=2,add = TRUE)




"EL BOX PLOT"

" ----------------- Vertical -------------------"

g1 = boxplot(e,main="DIAGRAMA DE CAJAS Y BIGOTES")

media = c(mean(e))

points(media, pch = 20, col = "red")


" -------- Horizontal -------------------------"
me = mean(e)
boxplot(e, main = "Gráfico de cajas y bigotes", horizontal = T)
points(me, pch = 8, col = "blue", y = T)



" ----------- GRÁFICO DE PROBABILIDAD NORMAL ---------------"
g1_qq = qqnorm(e, col="black",
               main = "Gráfico de probabilidad normal",
               pch = 8)

qqline(e, col = "red")



" ------ Gráfica de probabilidad normal con bandas de confianza ---------"
#Se requiere la librería 'car'

library(car)
qqPlot(e, pch=8, main="Gráfico de probabilidad normal\n para los errores de 
estimación", xlab="Percentiles teóricos", ylab ="Percentiles muestrales", col ="red", 
       col.lines = "black", lwd = 1)



"PRUEBA DE JARQUE - BERA CON EL PAQUETE tseries"
install.packages("tseries")
library(tseries)
p_jb_1 = jarque.bera.test(e)
p_jb_1



"VERIFICAR QUE LA MEDIA DE yt ES IGUAL A LA MEDIA DE yte"
"LA MEDIA DE yt ES"
(myt = mean(y))

"LA MEDIA DE yte ES"
(myte=mean(ye))



"La suma de cuadrados totales: TSS"
"TSS forma 1: Definición"

TSS = sum((y - mean(y)) ^ 2)
TSS


"TSS forma 2:"
TSS1 = (t(y)%*%y - n *(mean(y))^2)[1]
TSS1


"Como vector"
TSS1a = t(y)%*%y-n*(mean(y))^2
TSS1a


"Forma 3"
TSS2 = (n-1)*(sd(y))^2
TSS2


"La suma de cuadrados explicada"
ESS = (t(b)%*%XtX%*%b-n*(mean(y))^2)[1]
ESS


"Forma alternativa ESS1 = suma(yte-media(ye))^2"
ESS1 = sum((ye-mean(ye))^2)
ESS1


"La suma de cuadrados de los errores RSS"
RSS = sum(e^2)
RSS


"Forma Alternativa RSS = e'e"
RSS1=(t(e)%*%e)[1]
RSS1


" Verificación de la ecuación 16"
(TSS_E16=ESS+RSS)


" El coeficiente de determinación es"
R2 = ESS/TSS
R2


" ---- TABLA DE VERIFICACIÓN -----"

c1a = c("Estadística", "ESS", "ESS1", "RSS", "RSS1",
        "TSS", "TSS1", "TSS2", "TSS_E16", "Coef. Det.")

c2a = c("Valor", round(ESS,4), round(ESS1,4), round(RSS,4), round(RSS1,4), 
        round(TSS,4), round(TSS1,4), round(TSS2,4), round(TSS_E16,4), round(R2,4))
c2a


" ---- La descomposicón de Sumas de Cuadrados es ----"
(T_SQ = cbind(c1a,c2a))


# El modelo puede ser estimado con comados en R:

eq1 = lm(y ~ X2t + X3t +X4t)
summary(eq1)

eq2 = lm(y ~ X[,2:4])
summary(eq2)



"ESTIMACIÓN CON EL COMANDO lm()"
eq1=lm(y~X2t+X3t+X4t)
seq1=summary(eq1)
summary(eq1)


"De la ecuación 1 y del resumen de la ecuación se pueden obtener algunos elementos de 
interes y se hace así:"
"Los residuales de la ecuación"

et = resid(eq1)


"Residuales. Otra forma: Del resumen de la ecuación"
et_al=seq1$residuals


"Residuales de la la ecuación"
et_al1=eq1$residuals

"la suma de cuadrdos de errores es "
RSS=deviance(eq1)
RSS


"El estimador b de la ecuación 1"
be=cbind(coef(eq1))
be


"El estimador b del resumen de la ecuación"
be_r=cbind(seq1$coefficients)
be_r


"El coeficiente de determinación R2"
R2_e=seq1$r.squared
R2_e


"El coeficiente de determinación ajustado R2a"
R2a=seq1$adj.r.squared
R2a


"El AIC es"
aic=AIC(eq1)
aic


"El valor SC"
sc=BIC(eq1)
sc


"La matriz X del modelo se obtiene así"
Xm=model.matrix(eq1)
Xm
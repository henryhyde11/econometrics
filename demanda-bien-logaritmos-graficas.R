## Demanda de un bien con gráficas.

install.packages("carData")
install.packages("readxl")
install.packages("Rcpp")

library(Rcpp)
library(carData)
library(readxl)


datos = read_excel("C:/Users/USUARIO/econometrics/Datos_Demanda_Un_Bien.xls")
View(datos)

datos = data.frame(datos) 
attach(datos) 


k = 4 # N° de variables
n = 35 # N° de observaciones

y = log(datos$Y)
x2 = log(datos$X2)
x3 = log(datos$X3)
x4 = log(datos$X4)

datos = cbind(y, x2, x3, x4)
datos

x0 = as.matrix(cbind(1,datos[,2:4]))
x0

y0 = cbind(y)

x = x0[1:35,] #Matriz x
y = cbind(datos[1:35,2]) #Vector y

(n = length(y)) #Tamaño muestra

#Las variables son:

x2t = cbind(x[,2])
x3t = cbind(x[,3])
x4t = cbind(x[,4])

xtx = t(x)%*%x

xtx_inv = solve(xtx)

xty = t(x)%*%y

b = xtx_inv%*%xty #Estimador de minimos cuadrados

ye = cbind(x%*%b) #y estimado
ye

# Diagrama de dispersion Yt VS Yt estimado

plot(y, ye, type = "p", pch = 8, col="red",
     main = "Diagrama de dispersión\n Yt vs Yte", 
     xlab ="Ventas observadas",
     ylab="Ventas estimadas")

abline(a = 0, b = 1, lty = 2)


xf = x0[36:38,] # La matriz de pronósticos
xf

"Los pronósticos para los años 2006 al 2008"
yf = cbind(xf%*%b)
yf

# Tabla de pronósticos
c1 = c("Año", 2006, 2007, 2008)
c2 = c("Pronóstico", round(yf[1], 4), round(yf[2],4), round(yf[3],4))
T_P = cbind(c1,c2)
T_P


y0e = x0%*%b # El vector y0 estimado 
y0e


#Gráfica de pronósticos

plot(y0e, type = "l", main = "Grafica de los pronósticos",
     ylab = "Y0te - Yt0",
     xlab = "Indice",
     lty = 1, lwd = 1, ylim = c(40, 135))

lines(y0, col="red", lty = 2, lwd = 1)

e = y - ye #Errores de estimacion
e

summary(e)


#La suma de los errores de estimación es 0
me = round(mean(e), 4)
me

# x'e = 0
xte = round(cbind(t(x)%*%e),4)
xte

#PRUEBA DE NORMALIDAD:

#HISTOGRAMA DE RESIDUALES.

hist(e, seq(-6, 8, by = 2) , prob=TRUE,
     main = "Histograma de residuales",
     xlab = "Errores de estimación",
     ylab = "Freq. Relativa", xlim = c(-7,9))

curve(dnorm(x, mean = me, sd = sd(e)),
      col="red",lty=2,lwd=2,add = TRUE)

# BOx PLOT

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


"PRUEBA DE JARQUE - BERA CON EL PAQUETE tseries"

install.packages("tseries")
library(tseries)

jb = jarque.bera.test(e)
jb


#LA MEDIA DE yt ES IGUAL A LA MEDIA DE yte

(myt = mean(y)) #Media de yt

(myte=mean(ye)) #Media de yte




#Suma de Cuadrados Totales:

TSS = sum((y - mean(y)) ^ 2) #Forma 1
TSS

TSS1 = (t(y)%*%y - n *(mean(y))^2)[1] #Forma 2
TSS1


TSS2 = (n-1)*(sd(y))^2 #Forma 3
TSS2


#Suma de Cuadrados Explicada:

ESS = (t(b)%*%xtx%*%b-n * (mean(y))^2)[1] #Forma 1
ESS

ESS1 = sum((ye-mean(ye))^2) #Forma 2
ESS1


# Suma de Cuadrados de los Errores:

RSS = sum(e^2) #Forma 1
RSS

RSS1=(t(e)%*%e)[1] #Forma 2
RSS1


# Verificación de la ecuación 16:

TSS_E16 = ESS + RSS
TSS_E16

# Coeficiente de determinación es:

R2 = ESS/TSS
R2


" ---- TABLA DE VERIFICACIÓN -----"

c1a = c("Estadística", "ESS", "ESS1", "RSS", "RSS1",
        "TSS", "TSS1", "TSS2", "TSS_E16", "Coef. Det.")

c2a = c("Valor", round(ESS,4), round(ESS1,4), round(RSS,4), round(RSS1,4), 
      round(TSS,4), round(TSS1,4), round(TSS2,4), round(TSS_E16,4), round(R2,4))
c2a


#Descomposicón de Sumas de Cuadrados:

T_SQ = cbind(c1a, c2a)
T_SQ



# ---------------------------------------------
# lm()

eq1 = lm(y ~ x2t + x3t +x4t)
summary(eq1)


"Los residuales de la ecuación"

et = eq1$residuals
et


# Suma de Cuadrdos de Errores:
RSS = deviance(eq1)
RSS

# Estimador b

be = cbind(eq1$coefficients)
be


#AIC:
aic = AIC(eq1)
aic


#SC:
sc = BIC(eq1)
sc


#Matriz x del modelo
xm = model.matrix(eq1)
xm
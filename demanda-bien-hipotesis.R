#Demanda de un bien con hipotésis

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

y = datos$Y
x2 = datos$X2
x3 = datos$X3
x4 = datos$X4

datos = cbind(y, x2, x3, x4)
datos

x0 = as.matrix(cbind(1,datos[,2:4]))
x0

y0 = cbind(y)

x = x0[1:35,] #Matriz x

y = cbind(datos[1:35,1]) #Vector y

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

# ---------------------------------------------
# lm()

eq1 = lm(y ~ x2t + x3t +x4t)
summary(eq1)

S2 = RSS/ (n-k)
S2

#Pruebas de hipótesis.

"La matriz R = [ 1 0 0 0]"

(Rb1 = rbind(c(1,0,0,0)))

"El vector r"

(rb1 = cbind(c(0)))

## [1] " --- F ECUACIÓN (36) --------------"

qb1 = 1

Fc_36_b1 = (((t(Rb1%*%b-rb1)
              %*%solve(Rb1%*%xtx_inv%*%t(Rb1))
              %*%(Rb1%*%b - rb1))[1,1])/qb1)/S2 
Fc_36_b1


" El valor p de la prueba es"
pv_b1_36 = pf(Fc_36_b1, df1=qb1, df2 = n-k,
                   lower.tail = F)
pv_b1_36


## [1] " ------ F ECUACIÓN (38) ------"
"El intercepto estimado b1 es"
b[1]

"La varianza de b1 es"
S2b1 = S2[1]*xtx_inv
S2b1

" F de la ecuación (38)"

(Fc_38_b1 = (b[1]^2)/S2b1)

" El valor p es"
pv_b1_38 = pf(Fc_38_b1, df1=qb1, df2 = n-k,
                    lower.tail = F)
pv_b1_38



"El error estándar de b1 es"

Sb1 = S2[1]**(1/2)
Sb1  
  
#Estadística t ecuación (39):
(tb1 = b[1] / Sb1)

" El valor p para la significancia del intercepto es"
pb1 = 2*pt(abs(tb1), df = n-k,
                 lower.tail = F)
pb1


"Tabla de significancia del intercepto"

c1b1 = c("Estadistica", "Fc_36", "Fc_38", "tb1",
         "tb1^2", "Decisión sobre H0")


c2b1 = c("Valor", Fc_36_b1, Fc_38_b1, tb1, tb1^2,
       "La decisión es:")
c3b1 = c("Valor_p", pv_b1_36, pv_b1_38, pb1,
"_______", if(pb1>=0.05){"El 83
intercepto no es significativo"}else {"Intercepto significativo"})
"Los resultados son:"

sig_b1 = cbind(c1b1, c2b1, c3b1)
sig_b1

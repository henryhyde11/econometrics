#CAPITULO II
#ALGUNAS PRUEBAS PARA ERRORES DE ESPECIFICACIÓN EN EL MODELO DE REGRESIÓN LINEAL CON K – VARIABLES
# Ingreso-Ahorro

#install.packages("olssr")
#install.packages("lmtest")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("sandwich")
#install.packages("ggplot2")
#install.packages("tseries")
#install.packages("nortest")
#install.packages("strucchange")
#install.packages("readxl")
#install.packages("foreing")

library(olsrr)
library(lmtest)
library(dplyr)
library(tidyverse)
library(sandwich)
library(ggplot2)
library(tseries)
library(car)
library(nortest)
library(strucchange)
library(foreign)

"Libreria readxl para leer datos desde excel"
library(readxl) #Carga el paquete
Datos = read_excel("C:\Users\USUARIO\OneDrive - Universidad de Antioquia\Semestre 2023-1\Eco I\econometrics\Datos_Ejem2_Ingreso_Ahorro.xls")

Datos = data.frame(Datos) # Aqui se estable que Tiempo es una hoja de Excel
attach(Datos)
head(Datos)

"Las variables del modelo son"
Xt = Xt
Yt = Yt

"Tamaños muestrales"
n = 26
n1 = 15
n2 = 11
k = 2

alpha = 0.05

# El diagrama de dispersión de Yt en el tiempo

"--- Diagrama de dispersión Yt vs t  ------------"
"Se transforma Yt en una serie de tiempo usando la funcion ts()"
Yts = cbind(ts(Yt, start = 1970, end = 1995, frequency = 1)) # Esta función tiene cuatro argumentos principales: la serie a transformar, periodo inicial, periodo final y la frecuencia al año

"La serie es"
Yts

"Diagrama de dispersión del ahorro"
plot(Yts, type ="l", main = "Diagrama de dispersión", xlab = "Tiempo", ylab = "Ahorro" )



"Las variables para el modelo de la muestra 1 de tamaño n1 = 15: Modelo en el periodo 1970 - 1984"
X1t = Xt[1:15]
Y1t = Yt[1:15]

"Las variables para el modelo de la muestra 2 de tamaño n2 = 11: Modelo en el periodo 1985 - 1995"
X2t = Xt[16:26]
Y2t = Yt[16:26]

"---- Diagramas de dispersión de Yt vs Xt para los tres periodos -----"

# MODELO 1: PERIODO 1970 - 1995
g1 = plot(Xt, Yt, main = "Diagrama de dispersión\n Periodo  1970 - 1995", xlab = "Ingreso", ylab = "Ahorro", pch=8, col="red")
abline(lm(Yt~Xt), lty = 2)
par(mfrow=c(1,2))

# MODELO 2: PERIODO 1970 - 1984
g2 = plot(X1t, Y1t, main = "Diagrama de dispersión\n Periodo  1970 - 1984", xlab = "Ingreso", ylab = "Ahorro", pch=8, col="red")
abline(lm(Y1t~X1t), lty = 2)

# MODELO 3: PERIODO 1985 - 1995
g3 = plot(X2t, Y2t, main = "Diagrama de dispersión\n Periodo  1985 - 1995", xlab = "Ingreso", ylab = "Ahorro", pch=8, col="red")
abline(lm(Y2t~X2t), lty = 2)
par(mfrow=c(1,1))



"ESTIMACION DEL MODELO CON TODA LA MUESTRA"
"VALIDAR EL MODELO: SIGNIFICANCIA INDIVIDUAL, DE LA REGRESION, SIGNOS ESPERADOS, R2 Y PRUEBA DE NORMALIDAD"
eq1 = lm(Yt~Xt)
summary(eq1)

et = resid(eq1)
summary(et)

"El valor p"
pv_jb = pchisq(jb_c, df = 2, lower.tail = F )

"La decision"
if(pv_jb >= alpha) {
    "Se cumple el supuesto de normalidad"
} else {
        "No se cumple el supuesto de normalidad"
}


"-- Comparación de Yt con Yt estimado ---"
Yte=fitted(eq1)
plot(Yt, type = "l", main = "Comparación de Yt con Yt estimado", ylab = "Yt --- Yt_e")
lines(Yte,lty=2, col="red")
abline(v=15, col="blue", lty=2)
legend(x=1,y=250,legend = c("observado","estimado"),lty = c(1,2), col=c(1,2))

"Suma de cuadrados de los erroes del modelo de la ecuación (1)"
(RSS_R=deviance(eq1))

"Histograma con densidad normal"
g1_hist=hist(et, seq(-66.0, 74, by=20), prob=TRUE, main = "Histograma para et \n con distribucion normal", ylab = "Freq. Relativas")

curve(dnorm(x,mean = mean(et),sd=sd(et)),col="red3",lty=2,lwd=2,add = TRUE)

"Grafico de probabilidad normal"
library(car)
qqPlot(et, pch = 8, distribution = "norm", main = "Grafico de probabilidad normal", xlab = "Percentiles teoricos", ylab = "Percentiles de et", col = "red", col.lines = "black", lwd = 1)

library(tseries)
"Prueba de Jarque - Bera"
p_jb = jarque.bera.test(et)
p_jb

"El valor calculado es"
jb_c = p_jb$statistic
jb_c

"El valor p"
pv_jb=pchisq(jb_c, df = 2, lower.tail = F )
pv_jb

"La decision"
if(pv_jb >= alpha){
    "Se cumple el supuesto de normalidad"
} else {
    "No se cumple el supuesto de normalidad"
}

# Como extaer submuestras de los datos: se usa la subset(datos, criterio de seleccion).

"Datos antes de la crisis"
Datos1 = subset(Datos, date <= 1984)
Datos1

"Datos post crisis"
Datos2 = subset(Datos, date > 1984)
Datos2

"MUESTRA 1: CON LAS PRIMERA n1 = 15 Observaciones."
"DEFINIMOS LA MATRIZ X1 y EL VECTOR y1"
X1 = cbind(1,Datos[1:n1,3])
y1 = Datos[1:n1,2]

"DEFINIMOS LAS VARIABLES xt1 y y1"
eq1_m1 = lm(Y1t~X1t)
summary(eq1_m1)

"El RSS de este modelo es"
RSS1 = deviance(eq1_m1)
RSS1

"MUESTRA 2: CON LAS ultimas n2 = 11 Observaciones."
"DEFINIMOS LA MATRIZ X2 y EL VECTOR y2"
X2 = cbind(1,Datos2[,3])
y2 = Datos2[,2]

eq1_m2 = lm(Y2t~X2t)

summary(eq1_m2)

"El RSS de este modelo es"
RSS2 = deviance(eq1_m2)
RSS2


"PASO 1: ESTIMACIÓN DE b1 CON LA MUESTRA 1"
"LA MATRIZ X1tX1 ES:"
(X1tX1 = t(X1)%*%X1)

"EL VECTOR X1Ty1 ES"
(X1ty1 = t(X1)%*%y1)

"LA MATRIZ X1tX1_inv ES:"
(X1tX1_inv = solve(X1tX1))

"EL ESTIMADOR b1 ES"
(b1 = X1tX1_inv%*%X1ty1)

"SUMAS DE CUADRADOS MODELO MUESTRA 1"
"El y1 estimado es"
y1e = X1%*%b1

"Los errores de estimacion son"
e1t = y1 - y1e

"SUMAS DE CUADRADOS TOTALES"
(TSS1 = t(y1)%*%y1-n1*mean(y1)^2)

"SUMA DE CUADRADOS EXPLICADA"
(ESS1=t(b1)%*%X1tX1%*%b1-n1*mean(y1)^2)

"LA SUMA DE LOS CUADRADOS DE LOS ERRORES MODELO MUESTRA 1 ES"
"RSS1 "
(RSS1a = (t(e1t)%*%e1t)[1])

"El RSS1 es"
RSS1

"LA VARIANZA ESTIMADA PARA ESTE MODELO ES"
(S12 = RSS1a/(n1-k))


"LA MATRIZ X2 ES:"
X2=  cbind(1, Datos2[,3])
X2

"EL VARCTOR y2 ES"
y2 = cbind(Datos2[,2])
y2

"PASO 3"
"EL PRONÓSSTICO PARA y2 CON b1 DE DE LA ETAPA 1 ES:"
y2p = X2%*%b1
y2p

"COMPARACIÓN DE y2 CON y2p"
plot(y2, type="l", main="Comparación y2 vs y2p", ylab ="y2 - y2p", xlab = "Indice", ylim= c(150,420))
lines(y2p,lty=3, col = "red")
legend(x=1,y=400,legend = c("observado","pronostico"),lty = c(1,3), col = c(1,2))

"PASO 4"
"EL VECTOR d DE ERROES DE PRONÓSTICO"
d = y2-y2p
d

"La media del vector d es"
md = mean(d)
md


"ESTADISTICA DE PRUEBA F DE LA ECUACION 8"
"MATRIZ IDENTIDAD DE ORDEN n2=11"
IN2 = diag(n2)

"EL NUMERADO DE LA ESTADÍSTICA ES"
(a1 = t(d)%*%solve(IN2+X2%*%X1tX1_inv%*%t(X2))%*%d)

"EL Fc DE LA ECUACIÓN (8) ES"
(FC_8 = ((a1[1:1]/n2))/S12)

"EL P VALOR DE LA PRUEVA ES"
(PV_ECU_8 = pf(FC_8,11,n1-k,lower.tail = F))

"TABLA DE RESULTADOS"
C1 = c("El Fc ecu. 8", "El p-valor", "La decisión")
C2 = c(FC_8, PV_ECU_8, if(PV_ECU_8>=0.05) {"Hay estabilidad de parámetros"} else "Hay inestabilidad de parámetros")

"Tabla con la decisión"
Tabla_decision = cbind(C1, C2)
Tabla_decision


"LA SUMA DE CUADRADOS DE ERRORES DEL MODELO COMPLETO ES"
(RSS)

"LA SUMA DE CUADRADOS DE ERRORES DEL MODELO CON LAS PRIMERAS n1 OBS. ES"
(RSS1)

"LA VARIANZA ESTIMADA CON LA MUESTRA 1 ES:"
(S12)

"LA ESTADÍSTICA DE LA ECUACIÓN (9) ES"
(FC_9 = ((RSS-RSS1)/n2)/S12)

"LA ESTADÍSTICA DE LA ECUACIÓN (8) ES"
(FC_8)



rep = 23 # NUMERO DE REGRESIONES
Li_b1 = cbind(numeric(rep))
Ls_b1 = cbind(numeric(rep))
Li_b2 = cbind(numeric(rep))

Ls_b2 = cbind(numeric(rep))
v_b1 = cbind(numeric(rep))
v_b2 = cbind(numeric(rep))

for (irep in 4:n) {
    eq_i = lm(Yt[1:irep]~Xt[1:irep])
    seq_i = summary(eq_i)

    "El bi estimado"
    bi=cbind(coef(eq_i))
    b1i=bi[1]
    b2i=bi[2]

    "Los coeficientes recursivos para b1 son"
    v_b1[irep]=b1i

    "Los coeficientes recursivos para b2 son"
    v_b2[irep] = b2i
    var_b = vcov(eq_i)
    se_b1i = sqrt(var_b[1,1])
    se_b2i = sqrt(var_b[2,2])
    ta2i = qt(0.025, df = irep-k, lower.tail = F)
    
    "Intervalos de confianza del 95% para la Beta1"
    Li_b1[irep] = b1i-ta2i*se_b1i
    Ls_b1[irep] = b1i+ta2i*se_b1i
    
    "Intervalos de confianza del 95% para la Beta2"
    Li_b2[irep] = b2i-ta2i*se_b2i
    Ls_b2[irep] = b2i+ta2i*se_b2i
}


v_b1 = cbind(v_b1)
v_b1

"Intervalos para Beta 1"
(Int_Beta1 = cbind(Li_b1[4:26], v_b1[4:26], Ls_b1[4:26]))

"Intervalos para Beta 2"
(Int_Beta2 = cbind(Li_b2[4:26], v_b2[4:26], Ls_b2[4:26]))

"Grafico de coeficientes recursivos para Beta 1"
plot(Li_b1[5:26], type = "l", lty = 2 ,col ="red3", main = "Coeficientes recursivos para Beta 1", ylab = "Li - Ls", xlab = "Indice", ylim = c(-100,90))
lines(Ls_b1[5:26], type = "l", lty = 2, col="blue3")
lines(v_b1[5:26], type = "l", lty = 1, col = 1)
legend("bottomright", legend = c("Li","Ls", "b1"), col=c("red3", "blue3", 1), lty = c(2,2, 1))
grid()

"Grafico de coeficientes recursivos para Beta 2"
plot(Li_b2[5:26], type = "l", lty = 2, col ="red3", main = "Coeficientes recursivos para Beta 2", ylab = "Li - b2 - Ls", xlab = "Indice", ylim = c(-0.05, 0.2) )
lines(Ls_b2[5:26], type = "l", lty = 2, col="blue3")
lines(v_b2[5:26], type = "l", col = 1, lyt = 1)
legend("topright", legend = c("Li","Ls", "b2"), col=c("red3", "blue3", 1), lty = c(2,2,1))
grid()


rep = 23 # NUMERO DE REGRESIONES
v_ytei = cbind(numeric(rep))
vt_i = cbind(numeric(rep))
Li_vt = cbind(numeric(rep))
Ls_vt = cbind(numeric(rep))
v_b1 = cbind(numeric(rep))
v_b2 = cbind(numeric(rep))
comienzo = k + 2
comienzo

for (i in comienzo:n-1) {
    eq_i = lm(Yt[1:i]~Xt[1:i])
    seq_i = summary(eq_i)
    
    "El vector b estimado es"
    bi = cbind(coef(eq_i))
    
    "La matriz XtX_inv e"
    XtX_inv_i = seq_i$cov.unscaled
    
    "El SER es"
    SER_i = seq_i$sigma
    
    "La fila t+1 de X es"
    Xtfi = rbind(X[i+1,])
    
    "El y estimado recursivo es"
    yte_i = Xtfi%*%bi
    
    "Las estimaciones recursivas son"
    v_ytei[i] = yte_i
    
    "Los residuales recursivos son"
    et_i = Yt[i+1]-yte_i

    "El vector de residuales recursivos es"
    vt_i[i] =et_i
    
    "Los errores estandar de los et_i son"
    se_vt_i = SER_i*sqrt(1+(Xtfi%*%XtX_inv_i%*%t(Xtfi))[1])
    
    "El lmite inferior es"
    Li_vt[i] = -2*se_vt_i
    
    "El limite superior es"
    Ls_vt[i]=2*se_vt_i
}

"Los residuales recursivos con bandas de confianza son"
resid_recur = cbind(Li_vt[3:n-1], vt_i[3:n-1], Ls_vt[3:n-1])
resid_recur

"Grafico de residuales recursivos"
plot(Li_vt[3:n-1], type = "l", lty=2 ,col =2, main = "Residuales recursicos", ylab = "Li - vt - Ls", xlab = "Indice", ylim = c(-80,80))

lines(Ls_vt[3:n-1], type = "l", lty=2 ,  col=4)
lines(vt_i[3:n-1], type = "l", lty = 1, col = 1)
legend("topleft", legend = c("Li","Ls","vt"), col=c(2, 4, 1), lty = c(2,2,1))
grid()


install.packages("strucchange")
library(strucchange)

P_CUSUM = efp(Datos$Yt~Datos$Xt, type="Rec-CUSUM")
sctest(P_CUSUM)

plot(P_CUSUM, main = "Prueba CUSUM")
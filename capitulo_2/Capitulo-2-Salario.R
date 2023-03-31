#CAPITULO II

# Salario

#install.packages("readxl")
library(readxl) 

Datos = read_excel("C:\Users\USUARIO\OneDrive - Universidad de Antioquia\Semestre 2023-1\Eco I\econometrics\Datos_Ejemplo_3_salarios.xls")
Datos = data.frame(Datos) 
attach(Datos)
head(Datos
)
library(normtest)
library(car)

"Las variables del modelo es"
Yt = cbind(YT)
X2t = cbind(X2T)
X3t = cbind(X3T)

"ANÁLISIS GRÁFICO DEL SALARIO: LOS DATOS ESTÁN ORDENADOS DE FORMA QUE LAS PRIMERAS 15 OBSERVACIONES CORRESPONDEN AL SALARIO DE LOS HOMBRES Y LAS ULTIMAS 15 CORRESPONDE AL SALARIO PARA LAS MUJERES"
"DINÁMICA DEL SALARIO"
plot(Yt, type ="l", main = "Evolución del salario", xlab="índice", ylab="Salario", lwd =2)
abline(v=15, lty = 2, col="blue")

"VARIABLES PARA LOS HOMBRES"
y1h = cbind(Yt[1:15])
x2th = cbind(X2t[1:15])
x3th = cbind(X3t[1:15])

"VARIABLES PARA LOS MUJERES"
y2M = cbind(Yt[16:30])
x2tM = cbind(X2t[16:30])
x3tM = cbind(X3t[16:30])

" ---- Comparación salario de Hombres y mujeres -------"
plot(y1h, type="l", main="Comparación salarios hombre y mujeres",xlab = "Indice", ylab ="Salario")
lines(y2M,lty=3, col="red")
legend(x=1,y=3450,legend = c("Hombres","Mujeres"),lty = c(1,3), col=c(1,2))


"---------------- Variables para el modelo no restringido------------------"
uno_n1 = cbind(rep(1,15))
uno_n2 = cbind(rep(1,15))
cero_n1 = cbind(rep(0,15))
cero_n2 = cbind(rep(0,15))

D1a = rbind(uno_n1,cero_n2)
D1a

D2a = rbind(cero_n1,uno_n2)
D2a

"VARIABLES PARA ESTIMAR EL MODELO NO RESTRINGIDO"
X2tha = rbind(x2th,cero_n2)
X2tha

X3tha = rbind(x3th,cero_n2)
X3tha

X2tMa = rbind(cero_n1,x2tM)
X2tMa

X3tMa = rbind(cero_n1,x3tM)
X3tMa

"TAMAÑOS MUESTRALES"
n = 30
n1 = 15
n2 = 15
k = 3
q = k
ka = 2*k
alpha = 0.05


"LA MATRIZ DE DATOS X ES"
X = as.matrix(cbind(1,Datos[,3:4]))

"EL VECTOR y ES"
y = cbind(Datos[,2:2])

"LA MATRIZ DE DATOS PARA LOS HOMBRES ES"
X1 = as.matrix(X[1:15,])

"LA MATRIZ DE DATOS PARA LAS MUJERES ES"
X2 = as.matrix(X[16:30,])

"LA MATRIZ DE CEROS DE 15*3 ES"
O15 = matrix(nrow = 15, ncol = 3,0)

"LA MATRIZ XNR ES"
XNR = as.matrix(rbind(cbind(X1,O15),cbind(O15,X2)))

"LAS VARIABLES PARA ESTIMAR EL MODELO NO RESTRINGIDO SON"
D1 = cbind(XNR[,1])
D2 = cbind(XNR[,4])
X2ht = cbind(XNR[,2])
X3ht = cbind(XNR[,3])
X2mt = cbind(XNR[,5])
X3mt = cbind(XNR[,6])


"ESTIMACIÓN DEL MODELO NO RESTRINGIDO CON LA FUNCIÓN lm()"
eq1_mnr = lm(y ~ D1 + X2ht + X3ht + D2 + X2mt + X3mt -1)
seq1_mnr = summary(eq1_mnr)
summary(eq1_mnr)

et_mnr = resid(eq1_mnr)

"LA SUMA DE CUADRADOS DE LOS ERRORES DEL MODELO NO RESTRINGIDO ES"
"EL RSS_NR ES"
(RSS_NR = deviance(eq1_mnr))

"Histograma de residuales"
hist(et_mnr, prob = T, main = "Histograma de residuales", col = "gray", ylim = c(0,0.01))
curve(dnorm(x, mean = 0, sd=sd(et_mnr)), add = T, col="blue", lwd=2, lty=3)

"Prueba de probabilidad normal"
qqPlot(et_mnr, pch=8, main='Gráfico de probabilidad normal\n para los errores de estimación', xlab='Percentiles teóricos', ylab='Percentiles muestrales', col="red", lwd=1, col.lines = "black")

"Prueba de normalidad de Jarque - Bera"
p_jb = jarque.bera.test (et_mnr)
p_jb

"El valor calculado de la estadística Jarque - Bera es"
jb_c = p_jb$statistic
jb_c

"El valor p para la prueba es"
(pv_jb=pchisq(jb_c, df = 2, lower.tail = F))

"La decisión sobre normalidad en el modelo no restringido es"
if(pv_jb >= alpha){
    "La hipótesis de normalidad es válida"
} else {
    "No se cumple el supuesto de normalidad"
}


"ESTIMACIÓN EN FORMA MATRICIAL"
"LA MATRIZ XNR'XNR ES"
"XNRtXNR ES"
(XNRtXNR = t(XNR)%*%XNR)

"LA MATRIZ XNR'XNR INVERSA ES"
"XNRtXNR_INV ES"
(XNRtXNR_INV = solve(XNRtXNR))

"EL VECTOR XNR'y ES"
"XNRty"
(XNRty = t(XNR)%*%y)

"EL ESTIMADOR bnr ES"
(bnr = XNRtXNR_INV%*%XNRty)

" ------- Sumas de cuadrados -----------"
"EL y ESTIMADO DEL MODELO NO RESTRINGIDO ES"
ye_mnr = XNR%*%bnr

"LOS ERRORES DE ESTIMACIÓN DEL MODELO NO RESTRINGIDO ES"
et_nr = y - ye_mnr

"SUMAS DE CUADRADOS"
"EL TSS ES"
(TSS = sum((y-mean(y))^2))

"EL ESS DEL MODELO NO RESTRINGIDO ES"
"EL ESS_NR ES"
ESS_NR = (t(bnr)%*%XNRtXNR%*%bnr)[1]-n*(mean(y))^2
ESS_NR

"EL RSS_NR ES"
RSS_NR1 = (t(et_nr)%*%et_nr)[1]
RSS_NR1

"EL RSS_NR DE LA ECUACIÓN 1 ES"
RSS_NR

"prueba de TSS=ESS_NR+RSS_NR"
(TSS_p = ESS_NR+RSS_NR1)


"----------------MODELO PARA LOS HOMBRES-------------------"
eq1_h = lm(y1h~x2th+x3th)
seq1_h = summary(eq1_h)
summary(eq1_h)

"LA SUMA DE CUADRADOS DE LOS ERRORES PARA LOS HOMBRES ES"
(RSS1_h = deviance(eq1_h))

"EL ESTIMADOR PARA b PARA LOS HOMBRES ES"
(b1_h = cbind(coef(eq1_h)))


"-----------------MODELO PARA LAS MUJERES------------------"
eq1_m = lm(y2M~x2tM+x3tM)
seq1_m = summary(eq1_m)
summary(eq1_m)

"LA SUMA DE CUADRADOS DE LOS ERRORES PARA LAS MUJERES ES"
"EL RSS_2m ES"
(RSS2_m = deviance(eq1_m))

"EL ESTIMADOR PARA b PARA LAS MUJERES ES"
(b2_m = cbind(coef(eq1_m)))


"ESTADÍSTICA F DE LA ECUACIÓN (18)"
"LA MATRIZ IDENTIDAD DE ORDEN 3 ES"
(I3 = diag(3))

"LA MATRIZ R ES"
(R = cbind(I3,-I3))

"el vector r es"
(r = rbind(0,0,0))

"EL VALOR CALCULADO DE LA ESTADÍSTICA DE PRUEBA"
"EL Fc_8 ES"
(FC_8 = ((t(R%*%bnr-r)%*%solve(R%*%XNRtXNR_INV%*%t(R))%*%(R%*%bnr-r))[1]/3)/(RSS_NR/(n-ka)))

"el valor p de la prueba es"
(pv_ec_8=pf(FC_8, df1=q, df2=n-ka, lower.tail = F))


"---- LA DECISIÓN ----------------------"
c1 = c("ESTADÍSTICA", "FC_ECUACIÓN 8", "El valor p es", "Decisión sobre H0")
c2 = c("Valor", FC_8, pv_ec_8,
if(pv_ec_8>=0.05) {
    "Aceptamos H0: no hay cambio estructural"
} else {
    "Hay cambio estructural"
})



"Tabla de decisión"
T_D = cbind(c1,c2)
T_D


"ESTADÍSTICA F DE LA ECUACIÓN (19)"
"LA MATRIZ X'X INVERSA PARA LOS HOMBRES ES"
"LA MATRIZ X1tX1 INVERSA ES"
(X1HtX1H_INV = solve(t(X1)%*%X1))

"LA MATRIZ X'X INVERSA PARA LAS MUJERES ES"
"LA MATRIZ X2tX2 INVERSA ES"
(X2MtX2M_INV = seq1_m$cov.unscaled)

"LA ESTADÍSTICA F DE ECUACIÓN (19) ES"
"EL FC_19 ES"
(FC_19 = ((t(b1_h-b2_m)%*%solve(X1HtX1H_INV+X2MtX2M_INV)%*%(b1_h-b2_m))[1]/q)/(RSS_NR/(n-ka)))


"ESTADÍSTICA F DE LA ECUACIÓN (20)"
"EL MODELEO RESTRINGIDO ES"
eq1_r = lm(YT~X2T+X3T)
summary(eq1_r)

"LA SUMA DE LOS CUADRADOS DE LOS ERROES DEL MODELO RESNTINGIDO ES"
"EL RSS_R ES"
(RSS_R = deviance(eq1_r))

"EL RSS_NR ES"
RSS_NR

"EL VALOR CALCULADO DE LA ESTADÍSTICA F DE ECUACIÓN (20) ES"
(FC_20 = ((RSS_R-RSS_NR)/q)/(RSS_NR/(n-ka)))

"EL VALOR P ES"
(pv_ec_20 = pf(FC_20, df1=q, df2=n-ka, lower.tail = F))


"ESTIMACIÓN DEL MODELO NO RESTRINGIDO CON VARIABLES DUMMY"
eq1_mnr_d = lm(YT~D2+X2T+I(D2*X2T)+X3T+I(D2*X3T))
summary(eq1_mnr_d)

"EL ESTIMADOR bNR_D ES"
bnr_d = cbind(coef(eq1_mnr_d))
bnr_d

"LA SUMA DE LOS CUADRADOS DE LOS ERRORES ES"
"EL RSS_NR_D ES"
(RSS_NR_D = deviance(eq1_mnr_d))

"LA ESTADÍSTICA DE PRUEBA DE CAMBIO ESTRUCTURAL CON VARIABLES DUMMY"
"EL FC_22 ES"
(FC_22 = ((RSS_R-RSS_NR_D)/q)/(RSS_NR_D/(n-ka)))

"VERIFICACIÓN DE LOS COEFICIENTES"
b1d = bnr_d[1]
delta1 = bnr_d[2]
b2d = bnr_d[3]
delta2 = bnr_d[4]
b3d = bnr_d[5]
delta3 = bnr_d[6]

"TABLA DE VERIFICACIÓN"
c1c = c("Coef", "b1h", "b2h", "b3h", "b1m", "b2m", "b3m")
c2c = c("Mode. No Rest.", bnr[1], bnr[2], bnr[3], bnr[4], bnr[5], bnr[6])
c3c = c("Mode. DUMMY.", "b1d", "b2d", "b3d", "b1d+delta1","b2d+delta2", "b3d+delta3")
c4c = c("Valores", b1d, b2d, b3d, b1d+delta1,b2d+delta2, b3d+delta3)
"Tabla de coeficientes"

(T_coef = cbind(c1c,c2c,c3c,c4c))

"PRUEBA PARA CAMBIO SOLO EN EL INTERCEPTO"
"EL MODELO NO RESTRINGIDO PARA CAMBIO SOLO EN EL INTERCEPTO"
eq1_nr_inter = lm(y ~ D1 + D2 + X2T + X3T - 1)
summary(eq1_nr_inter)

kai = k + 1

qi = 1

"LA SUMA DE CUADRADOS DE LOS ERRORES DEL MODELO NO RESTRINGIDO PARA CAMBIO EN EL INTERCEPTO ES"
"EL RSS_NR_I ES"
(RSS_NR_I = deviance(eq1_nr_inter))

"LA ESTADÍSTICA DE PRRUEBA DE LA ECUACIÓN (24) ES"
"EL FC DE LA ECUACIÓN (24) ES"
(FC_24 = ((RSS_R-RSS_NR_I)/qi)/(RSS_NR_I/(n-kai)))

"EL VALOR P PARA LA PRUEBA ES"
(pv_ec_24 = pf(FC_24, df1=qi, df2 = n-kai, lower.tail = F))

"DECISIÓN"
if (pv_ec_24 >= 0.05) {
    "Aceptamos H0"
} else {
    "Rechazar H0: Concluir cambio en el intercepto"
}


"PRUEBA PARA CAMBIO SOLO EN LOS COEFICIENTES DE LA PENDIENTE"
"EL MODELO NO RESTRINGIDO PARA CAMBIO SOLO EN PENDIENTE"
eq1_nr_pend=lm(y~X2ht+X3ht+X2mt+X3mt)
summary(eq1_nr_pend)

kap = 2* (k - 1)

qp = k-1

"LA SUMA DE CUADRADOS DE LOS ERRORES DEL MODELO NO RESTRINGIDO PARA CAMBIOS EN LA PENDIENTE ES"
"EL RSS_NR_P ES"
(RSS_NR_P=deviance(eq1_nr_pend))

"LA ESTADÍSTICA DE PRRUEBA DE LA ECUACIÓN (27) ES"
"EL FC DE LA ECUACIÓN (24) ES"
(FC_27 = ((RSS_R-RSS_NR_P)/qp)/(RSS_NR_P/(n-kap)))

"EL VALOR P PARA LA PRUEBA ES"
(pv_ec_27 = pf(FC_27, df1=qp, df2 = n-kap, lower.tail = F))

"DECISIÓN"
if (pv_ec_27 >= 0.05) {
    "Aceptamos H0"
} else {
    "Rechazar H0: Concluir cambio en la pendiente"
}
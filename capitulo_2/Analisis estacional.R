#Análisis estacional.

library(readxl)
refrigeradores = read_excel("C:/Users/USUARIO/OneDrive - Universidad de Antioquia/Semestre 2023-1/Eco I/econometrics/data/refrigeradores.xlsx")

Datos=data.frame(refrigeradores) # Aqui se estable que Tiempo es una hoja de Excel
attach(Datos)
head(Datos)

"LA SERIE y ES"
y

"SE USA LA FUNCIÓN ts() PARA PARA TRANSFORMAR LA SERIE y EN UNA 
SERIE DE TIEMPO"
yt = ts(y,frequency = 4,start = c(1978,1), end = c(1985,4))

"LA SERIE y COMO UNA SERIE DE TIEMPO"
yt

"DIAGRAMA DISPERSIÓN PARA LAS VENTAS"
plot(yt, main="Diagrama de dispersión \n Ventas trimestrales", xlab="Tiempo", 
     ylab="Ventas")


"Variables estacionales"
s1 = ts(cbind(rep(c(1,0,0,0),8)),frequency = 4,start = c(1978,1), end = c(1985,4))
s2 = ts(cbind(rep(c(0,1,0,0),8)),frequency = 4,start = c(1978,1), end = c(1985,4))
s3 = ts(cbind(rep(c(0,0,1,0),8)),frequency = 4,start = c(1978,1), end = c(1985,4))
s4 = ts(cbind(rep(c(0,0,0,1),8)),frequency = 4,start = c(1978,1), end = c(1985,4))

"LA VARIABLE ESTACIONAL PARA EL PRIMER TRIMESTRE ES S1"
s1

"LA VARIABLE ESTACIONAL PARA EL SEGUNDO TRIMESTRE ES S2"
s2

"LA VARIABLE ESTACIONAL PARA EL TERCER TRIMESTRE ES S3"
s3

"LA VARIABLE ESTACIONAL PARA EL CUARTO TRIMESTRE ES S4"
s4

"ESTIMACIÓN DEL MODELO DE VENTAS CON VARIABLES DUMMY 
ESTACIONALES"
eq1 = lm(log(yt) ~ s1 + s2 + s3 + s4 - 1)
summary(eq1)

"LOS ERRORES DE ESTIMACIÓN DEL MODELO (1) SON LAS VENTAS 
DESESTACIONALIZADAS"
et1 = resid(eq1)
et = ts(et1,frequency = 4,start = c(1978,1), end = c(1985,4))

"Gráfico de las ventas desestacionalizadas"
plot(et,t="l", main="Serie de ventas desestacionalizada", xlab = "Tiempo", ylab = 
"Ventas desestacionalizadas")
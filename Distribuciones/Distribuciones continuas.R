#Considere que una variable aleatoria X se distribuye 
#beta con parametros a=2 y b=5.

curve(dbeta(x, shape1=2, shape2=5), lwd=3, las=1,
      ylab='Densidad f(t)')

curve(dbeta(x, shape1=2, shape2=15), lwd=3, las=1,
      ylab='Densidad')

curve(dbeta(x,2,5),add=T)



#P(191<xb<209) en una normal xb dis mui=200 sigmaxb=15/3=5
integrate(f=dnorm, lower=191, upper=209,200,5)

a1=1-pnorm(209,200,5)+pnorm(191,200,5)
b1=1-pnorm(191,200,5,lower.tail = F)+pnorm(209,200,5,lower.tail = F)



#Dibujo area sombreada en la normal

regionX1=seq(191,209,0.01)# Intervalo a sombrear

xP1=c(191,regionX1,209) # Base de los poligonos 
#que crean el efecto "sombra"


yP1=c(0,dnorm(regionX1,200,5),0) # Altura de los poligonos sombreados
curve(dnorm(x,200,5),xlim=c(180,220),yaxs="i",ylim=c(0,.1),ylab="f(x)",
      main='Densidad Beta(2,5)') 
polygon(xP1,yP1,col="2")


#Calcular P(0.3≤X≤0.7).
integrate(f=dbeta, lower=0.3, upper=0.7,
          shape1=2, shape2=5)
#Otra forma de obtener la probabilidad solicitada es restando de F(xmax) la probabilidad F(xmin). Las probabilidades acumuladas hasta un valor dado se obtienen con la funcion pbeta, a continuacion el codigo necesario.
a1=pbeta(q=0.7, shape1=2, shape2=5) - pbeta(q=0.3, shape1=2, shape2=5)

#Dibujo area sombreada en la beta
regionX1=seq(.3,.7,0.01)# Intervalo a sombrear
xP1=c(.3,regionX1,.7) # Base de los poligonos que crean el efecto "sombra"
yP1=c(0,dbeta(regionX1,2,5),0) # Altura de los poligonos sombreados
curve(dbeta(x,2,5),xlim=c(0,1),yaxs="i",ylim=c(0,2.5),ylab="f(x)",
      main='Densidad Beta(2,5)') 
polygon(xP1,yP1,col="2")

curve(dbeta(x, shape1=2, shape2=5), lwd=3, las=1,ylim=c(0,10),
      ylab='Densidad f(t)',col="3")
curve(pbeta(x, shape1=2, shape2=5), lwd=3, las=1,add=T,ylab='Densidad f(t)',col="5")

curve(1-pbeta(x, shape1=2, shape2=5), lwd=3, las=1,add=T,col="10",
      ylab='Densidad f(t)')

curve(dbeta(x, shape1=2, shape2=5)/(1-pbeta(x, shape1=2, shape2=5)),col="15" ,lwd=3, las=1,add=T,
      ylab='Densidad f(t)')
n=10000
a=rbeta(n,2, 2, ncp = 0)

curve(dgamma(x,2,2), lwd=1, las=1,xlim=c(0,10),ylim=c(0,2),
      ylab='Densidad f(t)',col="3")
curve(pgamma(x,2,2), lwd=2, las=1,add=T,ylab='Densidad F(t)',col="5",ylim=c(0,2))

curve(1-pgamma(x,2,2), lwd=2, las=1,add=T,col="10",
      ylab='Densidad R(t)')

curve(dgamma(x,2,2)/(1-pgamma(x,2,2)),col="15" ,ylim=c(0,2),lwd=2, add=T,ylab='Densidad z(t)')

#Binomial b(10,.25)
x=seq(0,10,1) # generamos el soporte de la distribucion  
y=dbinom(x, size=10, prob=.25) # evaluamos las probabilidades 
plot(x, y, type = "h", lwd = 3, main = "Probabilidades Binomiales con n = 10, p = .25", col = "gray")
plot(0:10, dbinom(0:10, size=10, prob=.25),xlab="x", ylab="y", type = "h", lwd = 20, main = "Probabilidades Binomiales con n = 10, p = .25", col = "gray")
barplot(y,names.arg=as.character(0:10),main = "Probabilidades Binomiales con n = 10, p = .25", xlab="x")
#Normal estandar
curve(dnorm(x), from = -4, to = 4) # curva normal estandar
curve(pnorm(x, mean=10, sd=2), from = 4, to = 16) # CDF normal
curve(dnorm(x,170,12),xlim=c(130,210),col="blue",lwd=2,
      xlab="x",ylab="f(x)",main="Funcion de Densidad N(170,12)")
curve(pnorm(x,170,12),xlim=c(130,210),col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Funcion de Distribucion N(170,12)")
miDensidad=function(x) dnorm(x,170,12)
integrate(miDensidad,150,168)#P(150<X<168)
a2=pnorm(168,170,12,lower.tail = T)-pnorm(150,170,12,lower.tail = T)
a3=pnorm(150,170,12,lower.tail = F)-pnorm(168,170,12,lower.tail = F)

#Dibujo area sombreada en la normal 
regionX=seq(150,168,0.01)# Intervalo a sombrear
xP=c(150,regionX,168) # Base de los poligonos que crean el efecto "sombra"
yP=c(0,dnorm(regionX,170,12),0) # Altura de los poligonos sombreados
curve(dnorm(x,170,12),xlim=c(130,210),yaxs="i",ylim=c(0,0.035),ylab="f(x)",
      main='Densidad N(170,12)') 
polygon(xP,yP,col="6")


#Distribucion exponencial
curve(dexp(x,rate=1/3),from=-3,to=10)
z=seq(-3,10,0.1)
#Con  la  opcion  type=conseguimos  dibujar  el  grafico como una linea
plot(z,dexp(z,rate=1/3),type="l")
# Dibujamos ahora la funcion de distribucion
curve(pexp(x,rate=1/3),from=-3,to=10)

#Tomamos como densidades la gamma de parametros Alfa = 20
#y r = 10 y la normal de parametros μ = 0 y sigma= 1, 
#podemos representar la funcion de densidad conjunta mediante: mediante:
lam=20
r=10
x=seq(0,5,.05)
mu=0
sig=1
y=seq(-5,5,.05)
persp(x, y, outer(x,y,function(x,y){dgamma(x,lam,r)*dnorm(y,mu,sig)}),
        theta = 30,phi = 30, expand = 0.5, col = "lightblue",
        main="X: Gamma(20,10) Y: Normal(0,1)")
#Dibujamos  la  funcion  de  masa  de  la  distribucion  B(500,0.1)  y  le  superponemos  la  funcion de densidad de la N(media,destip) 
z=0:100
plot(z,dbinom(z,500,0.1),type="h")
curve(dnorm(x,50,45^.5),add=T)
#Notemos que  en  la  sentencia  anterior  hemos  utilizado la opcion 
# add=T (TRUE) para indicarle a R que superponga este grafico al anterior

#Ejercicio 6.62
curve(dexp(x,rate=6),from=-2,to=2)
z=seq(-2,2,0.1)
#Con  la  opcion  type=conseguimos  dibujar  el  grafico como una linea
plot(z,dexp(z,rate=6),type="l")
# Dibujamos ahora la funcion de distribucion
curve(pexp(x,rate=6),from=-2,to=2)
a4=pexp(.25,6,lower.tail = F)
a5=1-pexp(.25,6,lower.tail = T)

#Ejercicio 6.42
curve(dgamma(x,2,2),from=-1,to=6)
z=seq(-1,15,0.1)
#Con  la  opcion  type= conseguimos  dibujar  el  grafico como una linea
plot(z,dgamma(z,2,2),type="l")
# Dibujamos ahora la funcion de distribucion
curve(pgamma(x,2,2),from=-1,to=15)
a6=pgamma(1,2,2,lower.tail = T)
a5=pgamma(2,2,2,lower.tail = F)
#Dibujo area sombreada en la gamma
regionX1=seq(0,1,0.01)# Intervalo a sombrear
xP1=c(0,regionX1,1) # Base de los poligonos que crean el efecto "sombra"
yP1=c(0,dgamma(regionX1,2,2),0) # Altura de los poligonos sombreados
curve(dgamma(x,2,2),xlim=c(0,10),yaxs="i",ylim=c(0,.8),ylab="f(x)",
      main='Densidad Beta(2,5)') 
polygon(xP1,yP1,col="2")




#Ejercicio 6.10
curve(dnorm(x,0,1),from=-5,to=5)
z=seq(-5,5,0.1)
#Con  la  opcion  type=conseguimos  dibujar  el  grafico como una linea
plot(z,dnorm(z,0,1),type="l")
# Dibujamos ahora la funcion de distribucion
curve(pnorm(x,0,1),from=-5,to=5)
a6=pnorm(3,0,1,lower.tail = T)-pnorm(-3,0,1,lower.tail = T)


#Ejercicio 6.86
curve(dlnorm(x,1.8,2),from=-1,to=25)
z=seq(-1,25,0.1)
#Con  la  opcion  type=conseguimos  dibujar  el  grafico como una linea
plot(z,dlnorm(z,1.8,2),type="l")
# Dibujamos ahora la funcion de distribucion
curve(plnorm(x,1.8,2),from=-1,to=25)
a6=plnorm(20,1.8,2,lower.tail = F)
a7=plnorm(60,1.8,2,lower.tail = F)
a8=plnorm(44.7,1.8,2,lower.tail = T)


#Ejercicio 2.1
t=seq(0,10,2)
integrand <- function(x) {1/((x+1)*sqrt(x))}

R=function(t){1/(.2*t+1)^2}
f=function(t){1/(.2*t+1)^3}
z=function(t){z=1/(.2*t+1)}
plot(R,type="l",col="red",xlim=c(0,10));text(10, 20, "Curva R")
curve(f,add=T,col="3",xlim=c(0,10));text(10, 20, "Curva f")
curve(z,add=T,col="8",xlim=c(0,10));text(10, 20, "Curva z")
l = expression(paste("R(t)"), paste("f(t)"), paste("z(t)"))
legend("topright",          # Posici?n
       legend = l,          # Expression vector anterior
       lty = c(1, 3, 5), # L?neas s?lidas
       bty = "n",           # Sin bordes
       col = c("red", "green", "blue"),
       inset = .05,         # Espaciado del margen
       y.intersp = .75)     # Interlineado

#P(X>a)=.15 x es normal(24,3.8)
a1=qnorm(.15,24,3.8,lower.tail = F)
#Dibujo area sombreada en la normal
regionX1=seq(a1,45,0.1)# Intervalo a sombrear
xP1=c(a1,regionX1,45) # Base de los poligonos que crean el efecto "sombra"
yP1=c(0,dnorm(regionX1,24,3.8),0) # Altura de los poligonos sombreados
curve(dnorm(x,24,3.8),xlim=c(15,45),yaxs="i",ylim=c(0,.15),ylab="f(x)",
      main='Densidad normal(24,3.8)') 
polygon(xP1,yP1,col="60")

#ejercicio 6.28 x es binom(80,0.75) x es normal miu=np=80*.75 desv=(np(1-p))=(80*.75*.25)^.5
x=0:80 # generamos el soporte de la distribucion  
y=dbinom(x, size=80, prob=.75) # evaluamos las probabilidades 
plot(x, y, type = "l", lwd = 10, main = "Probabilidades Binomiales con n = 80, p = .75", col = "gray")
plot(0:80, dbinom(0:80, size=80, prob=.75),xlab="x", ylab="y", type = "h", lwd = 10, main = "Probabilidades Binomiales con n = 10, p = .25", col = "gray")
barplot(y,names.arg=as.character(0:80),main = "Probabilidades Binomiales con n = 10, p = .25", xlab="x")

a1=80*.75
b1=(80*.75*.25)^.5
regionX1=seq(49.5,80,0.5)# Intervalo a sombrear
xP1=c(49.5,regionX1,80) # Base de los poligonos que crean el efecto "sombra"
yP1=c(0,dnorm(regionX1,a1,b1),0) # Altura de los poligonos sombreados
curve(dnorm(x,a1,b1),xlim=c(40,80),yaxs="i",ylim=c(0,.15),ylab="f(x)",
      main='Densidad normal(24,3.8)') 
polygon(xP1,yP1,col="60")
#P(falle)=P(X>10) o P(X<-10)  P(-10<=X<=10)
a2=pnorm(-10,0,4,lower.tail = T)+pnorm(10,0,4,lower.tail = F)
pexp(200,.01)

a3=pweibull(2,1/2,1/2,lower.tail = F)
regionX2=seq(2,10,.01)# Intervalo a sombrear
xP2=c(2,regionX2,10) # Base de los poligonos que crean el efecto "sombra"
yP2=c(0,dweibull(regionX2,1/2,1/2),0) # Altura de los poligonos sombreados
curve(dweibull(x,1/2,1/2),xlim=c(0,10),yaxs="i",ylim=c(0,1),ylab="f(x)",
      main='Densidad normal(24,3.8)') 
polygon(xP2,yP2,col="60")

#bimon negativa 8,2,1/6
regionX2=seq(2,10,.01)# Intervalo a sombrear
xP2=c(2,regionX2,10) # Base de los poligonos que crean el efecto "sombra"
yP2=c(0,dweibull(regionX2,1/2,1/2),0) # Altura de los poligonos sombreados
curve(dnbimon(x,2,1/6),xlim=c(0,100),yaxs="i",ylim=c(0,1),ylab="f(x)",
      main='Densidad normal(24,3.8)') 
polygon(xP2,yP2,col="60")
z1=seq(0,100,1)
bnz1=dnbinom(z1,2,1/6)
plot(z1,bnz1,type = "l")


















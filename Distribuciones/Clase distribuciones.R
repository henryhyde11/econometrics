

#Podemos representar facilmente la funcion de probabilidad de la distribucion binomial:b(15,0.4)
a=dbinom(0,15,.4)

plot(dbinom(0:15,15,0.4),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad B(15,0.4)")
#También podemos representar su funcion de distribucion:?
plot(stepfun(0:15,pbinom(0:16,15,0.4)),xlab="k",ylab="F(k)",main="(Funcion de distribucion B(15,0.4)")
a1=pbinom(10,15,.4,lower.tail=F)
a2=1-pbinom(9,15,.4,lower.tail=T)
a3=pbinom(8,15,.4,lower.tail=T)-pbinom(2,15,.4,lower.tail=T)
a3
dbinom(5,15,.4)
b=pbinom(5,15,.4,lower.tail=T)-pbinom(4,15,.4,lower.tail=T)
#mostrar b(x,20,.03)
plot(dbinom(0:20,20,0.03),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad B(20,0.03)")
plot(stepfun(0:20,pbinom(0:21,20,0.03)),xlab="k",ylab="F(k)",main="Funcion de distribucion B(20,0.03)")
x1=seq(0,20,1)
x2=dbinom(x1,20,.03)
plot(x2)
1-pbinom(0,20,.03,lower.tail = T)

#Si simulamos una muestra muy grande de esta distribucion (por ejemplo, 10.000 valores), podemos comprobar como las frecuencias relativas son muy similares a las probabilidades teoricas:
X=rbinom(10000, 15, 0.4)
X
freqAbs=table(X)
freqAbs
freqRel=prop.table(freqAbs)
freqRel
#Vamos a mostrar lado a lado las frecuencias relativas y las probabilidades teoricas. Para ello, generamos primero un dataframe con la tabla de probabilidades teoricas:
probsTeo=data.frame(X=0:15,Prob=dbinom(0:15,15,0.4))
probsTeo
#Para presentar en una unica tabla las probabilidades teoricas y las frecuencias relativas de nuesta simulacion, podemos utilizar la funcion merge. Esta funcion combina dataframes que tengan un campo en comun (en este caso el valor de la variable X). El objeto probsTeo es obviamente un dataframe. 
# Ahora bien, mediante class(freqRel) podemos comprobar que freqRel es un objeto de clase table. Para poder combinarlo con probsTeo hemos de convertirlo primero en dataframe:
freqRel=as.data.frame(freqRel)
str(freqRel)
#Vemos aqui que la variable X es de tipo factor; para poder 
# combinar bien este dataframe con probsTeo hemos de convertir X a la clase integer (ya que en ese dataframe, X es entera, como puede comprobarse mediante str(probsTeo)):
freqRel$X=as.integer(as.character(freqRel$X))
#Por ultimo procedemos a combinar ambos objetos mediante la funcion merge y mostramos el resultado:
compara=merge(freqRel,probsTeo,all=TRUE)
compara
#Podemos hacer un grafico que nos muestre la similitud entre probabilidad y frecuencia relativa:
with(compara,{
   plot(X,Freq, type="b")
   points(X,Prob,col="red",pch=4)
   lines(X,Prob,col="red",lty=2,lwd=2)
   legend("topleft",c("frec. relativa","probabilidad"),col=c("black","red"),lty=1:2,pch=c(1,4))
})

plot(dbinom(0:20,20,0.3),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad B(20,0.3)")
plot(stepfun(0:20,pbinom(0:21,20,0.3)),xlab="k",ylab="F(k)",main="Funcion de distribucion B(20,0.3)")
x=seq(0,20,1)
dx=dbinom(x,20,.3)
dx
1-pbinom(9,20,.3,lower.tail = T)
pbinom(4,20,.3)
dbinom(5,20,.3)
qbinom(c(.25),20,.3)
a=rbinom(100000,20,.3)
a

#Podemos calcular facilmente los valores de la funcion de densidad sobre una secuencia de valores de x:
x=seq(165,175,by=0.5)
dnorm(x,170,12)
#La representacion grafica de la funcion de densidad se obtiene facilmente como:
curve(dnorm(x,170,12),xlim=c(130,210),col="blue",lwd=2,
      xlab="x",ylab="f(x)",main="Funcion de Densidad N(170,12)")
#También podemos representar la funcion de distribucion:
curve(pnorm(x,170,12),xlim=c(130,210),col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Funcion de Distribucion N(170,12)")
#No es demasiado difacil representar el area correspondiente a la probabilidad que se acaba de calcular:
regionX=seq(150,168,0.01)            # Intervalo a sombrear
xP <- c(150,regionX,168)             # Base de los polagonos que crean el efecto "sombra"
yP <- c(0,dnorm(regionX,170,12),0)   # Altura de los polagonos sombreados
curve(dnorm(x,170,12),xlim=c(130,210),yaxs="i",ylim=c(0,0.035),ylab="f(x)",
      main='Densidad N(170,12)') 
polygon(xP,yP,col="orange1")
box()
#Simulamos una muestra grande de la distribucion normal y comprobamos que el histograma es muy parecido a la funcion de densidad:
X=rnorm(10000, 170, 12)
hist(X,freq=FALSE,col="lightsalmon",main="Histograma",sub="Datos simulados de una N(170,12)")
curve(dnorm(x,170,12),xlim=c(110,220),col="blue",lwd=2,add=TRUE)
#Podemos comprobar también que la distribucion acumulativa emparica de esta simulacion es muy similar a la funcion de distribucion teorica de la normal:
plot(ecdf(X))
curve(pnorm(x,170,12),xlim=c(110,220),col="red",lwd=2,lty=2,add=TRUE)
legend("topleft",lty=c(1,2),lwd=c(1,2),col=c("black","red"),legend=c("emparica","teorica"))





#R: Distribucion Hipergeométrica.
#dhyper(x, m, n, k, log = F)	Devuelve resultados de la funcion de densidad.
#phyper(q, m, n, k, lower.tail = T, log.p = F)	Devuelve resultados de la funcion de distribucion acumulada.
#qhyper(p, m, n, k, lower.tail = T, log.p = F)	Devuelve resultados de los cuantiles de la Hipergeométrica.
#rhyper(nn, m, n, k)	Devuelve un vector de valores de la Hipergeométrica aleatorios.

#ejercicio 5.29
#Podemos representar facilmente la funcion de probabilidad de la distribucion Hipergeometrica:hyper(x,100,10,12)
#x valor a calcular, m: defectusos en el lote n:buenos en el lote k:tamaño muestra
plot(dhyper(0:4,4,5,6),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad hyper(x,4,5,6)")
#También podemos representar su funcion de distribucion:?
plot(stepfun(0:4,phyper(0:5,4,5,6)),xlab="k",ylab="F(k)",main="Funcion de distribucion hyper(x,100,10,12)")

dhyper(2,4,5,6)
a=5/14
1-phyper(0,6,9,3)
phyper(0,6,9,3,lower.tail =F)
b=53/65
#ejercicio 5.30
#Podemos representar facilmente la funcion de probabilidad de la distribucion Hipergeometrica:hyper(x,100,10,12)
#x valor a calcular, m: defectusos en el lote n:buenos en el lote k:tamaño muestra
plot(dhyper(0:3,6,9,3),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad hyper(x,4,5,6)")
#También podemos representar su funcion de distribucion:?
plot(stepfun(0:3,phyper(0:4,6,9,3)),xlab="k",ylab="F(k)",main="Funcion de distribucion hyper(x,100,10,12)")

#ejercicio 5.38
#Podemos representar facilmente la funcion de probabilidad de la distribucion Hipergeometrica:hyper(x,100,10,12)
#x valor a calcular, m: defectusos en el lote n:buenos en el lote k:tamaño muestra
plot(dhyper(0:30,300,9700,30),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad hyper(x,4,5,6)")
#También podemos representar su funcion de distribucion:?
plot(stepfun(0:30,phyper(0:31,300,9700,30)),xlab="k",ylab="F(k)",main="Funcion de distribucion hyper(x,100,10,12)")
b=pbinom(13,18,.7,lower.tail = T)-pbinom(9,18,.7,lower.tail = T)

phyper(2,30,120,10,lower.tail = F)#P(X>x)
p=30/150
b=1-pbinom(2,10,.2,lower.tail = T)
#Podemos representar facilmente la funcion de probabilidad de la distribucion binomial:b(15,0.4)
dbinom(0,15,.4)

plot(dbinom(0:18,18,0.7),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad B(15,0.4)")
#También podemos representar su funcion de distribucion:?
plot(stepfun(0:18,pbinom(0:19,18,0.7)),xlab="k",ylab="F(k)",main="Funcion de distribucion B(15,0.4)")

c=phyper(0,300,9700,30,lower.tail = F)
ppois(3,2,lower.tail = F)
dpois(0,2)
dhyper(1,5,35,3)
dhyper(3,12,88,10)
dhyper(3,1000,4000,10)
dnbinom(6,4,.55)
dpois(6,4)
1-ppois(15,10, lower.tail = T)
plnorm(50000,5,2,lower.tail = F)
pgamma(12,2,1/5,lower.tail = F)


#ejercicio 5.66
#Podemos representar facilmente la funcion de probabilidad de la distribucion Hipergeometrica:hyper(x,100,10,12)
plot(dpois(0:150,6),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad hyper(x,4,5,6)")
#También podemos representar su funcion de distribucion:?
plot(stepfun(0:150,ppois(0:151,6)),xlab="k",ylab="F(k)",main="Funcion de distribucion hyper(x,100,10,12)")
pgeom(50,.2,lower.tail = F)
#Podemos representar facilmente la funcion de probabilidad de la distribucion Hipergeometrica:hyper(x,100,10,12)
plot(dgeom(0:150,.2),type="h",xlab="k",ylab="P(X=k)",main="Funcion de Probabilidad hyper(x,4,5,6)")
#También podemos representar su funcion de distribucion:?
plot(stepfun(0:150,pgeom(0:151,.2)),xlab="k",ylab="F(k)",main="Funcion de distribucion hyper(x,100,10,12)")
#pnorm dada x encontrar el area
#qnorm dado area encontrar la x
qnorm(.3622,0,1,lower.tail = F)
qnorm(.1131,0,1,lower.tail = T)
qnorm(.5-.4838,0,1,lower.tail = F)
qnorm(.025,0,1,lower.tail = F)


x=rnorm(15000,170,12)
x
y=matrix(x,ncol=1000)
dim(y)
z=apply(y,1,mean)
z
dim(z)
w=apply(y,1,sd)
w
freqAbs=table(X)
freqAbs
freqRel=prop.table(freqAbs)
freqRel
hist(X,freq=FALSE,col="lightsalmon",main="Histograma",sub="Datos simulados de una N(170,12)")
curve(dnorm(x,170,12),xlim=c(110,220),col="blue",lwd=2,add=TRUE)
#Podemos comprobar también que la distribucion acumulativa emparica de esta simulacion es muy similar a la funcion de distribucion teorica de la normal:
plot(ecdf(X))
curve(pnorm(x,170,12),xlim=c(110,220),col="red",lwd=2,lty=2,add=TRUE)
legend("topleft",lty=c(1,2),lwd=c(1,2),col=c("black","red"),legend=c("emparica","teorica"))

hist(y,freq=FALSE,col="lightsalmon",main="Histograma",sub="Datos simulados de una N(170,12)")
curve(dnorm(y,170,12),xlim=c(110,220),col="blue",lwd=2,add=TRUE)
#Podemos comprobar también que la distribucion acumulativa emparica de esta simulacion es muy similar a la funcion de distribucion teorica de la normal:
plot(ecdf(y))
curve(pnorm(x,170,12),xlim=c(110,220),col="red",lwd=2,lty=2,add=TRUE)
legend("topleft",lty=c(1,2),lwd=c(1,2),col=c("black","red"),legend=c("emparica","teorica"))
#X1 DIST N(400,100)
#X2 DIST N(800,250)
#X3 DIST N(100,40) VAR(T)=100^2+250^2+40^2 SIGMAT=
SVART_T=(100^2+250^2+40^2)^.5
# T=X1+X2+X3  T DIST N(1300,SVAR_T)
PUNA=pnorm(1600,1300,SVART_T,lower.tail = T)





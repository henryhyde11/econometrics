x = rnorm(1e5, 1, 2)
x

xks = ks.test(x, 'pnorm')
xks

Ksn2 = ks.test(x,'pnorm', mean = 1, sd = 2)
Ksn2



install.packages("fitdistrplus")
library("fitdistrplus")

plotdist(x, histo = TRUE, demp = TRUE)



##
xc = c(2.2,4.1,3.5,4.5,3.2,3.7,3.0,2.6,3.4,1.6,3.1,3.3,
       3.8,3.1,4.7,3.7,2.5,4.3,3.4,3.6,2.9,3.3,3.9,3.1,
       3.3,3.1,3.7,4.4,3.2,4.1,1.9,3.4,4.7,3.8,3.2,2.6,
       3.9,3.0,4.2,3.5)

plotdist(xc, histo = TRUE, demp = TRUE)

Ksn3 = ks.test(xc, "pnorm", mean=3.5, sd=0.7)
Ksn3


library("tseries")

jarque.bera.test(xc)

qqnorm(xc, pch = 19, col = "8")

qqline(xc)



#Pruebas de independencia chi-2
tablaObservada = matrix( c(182,213,203,154,138,110),  nrow= 2, byrow = TRUE)
colnames(tablaObservada) = c("Bajo", "Medio","Alto")
rownames(tablaObservada) = c("A Favor", "En Contra" )
tablaObservada
chisqTest = chisq.test(tablaObservada, correct=FALSE)
chisqTest
#Ejercicio 10.82
y=seq(0,3,1)
foy=c(1,31,55,25)
#Hipergeometrica h(5,3,3)
pey=dhyper(y,5,3,3)
fey=112*pey

#Pruebas de homogeneidad chi-2 
to = matrix( c(65,71,78,82,35,29,22,18),  nrow= 2, byrow = TRUE)
colnames(to) = c("MD", "VA","GA","AL")
rownames(to) = c("Si", "No" )
to
chito = chisq.test(to, correct=FALSE)
chito


# Funci?n de densidad de probabilidad
# Creamos la curva inicial normal
curve(dnorm(x, 0, sqrt(0.2)), # Funci?n dnorm a evaluar
      -5, 5, 1000, # L?mites de x y n? de valores a evaluar
      col = "red", 
      las = 1, # Etiquetas alineadas horizontalmente
      ann = FALSE, # Sin t?tulos en los ejes
      xaxp = c(-5, 5, 10),  # Marcas del eje x
      ylim = c(0,1), # L?mites del eje
      yaxs = "i") # Estilo del eje y, ajustado a los l?mites
# A?adimos el resto de curvas
curve(dnorm(x), add = TRUE, col = "green")
curve(dnorm(x, 0, sqrt(5)), add = TRUE, col = "blue")
curve(dnorm(x, -2, sqrt(0.5)), add = TRUE, col = "pink")

# Le a?adimos la leyenda con las letras griegas: ? (mi, mu en ingl?s) y s (sigma).
# Lista (expression vector) con los textos de la leyenda
l <- expression(paste(mu, "= 0, ", sigma^2, "= 0.2"), paste(mu, "= 0, ", sigma^2, "= 1.0"), paste(mu, "= 0, ", sigma^2, "= 5.0"), paste(mu, "=-2, ", sigma^2, "= 0.5"))
legend("topright",          # Posici?n
       legend = l,          # Expression vector anterior
       lty = c(1, 1, 1, 1), # L?neas s?lidas
       bty = "n",           # Sin bordes
       col = c("red", "green", "blue", "pink"),
       inset = .05,         # Espaciado del margen
       y.intersp = .75)     # Interlineado
# Para a?adir l?neas de divisi?n
grid()


# Funci?n de distribuci?n acumulada de probabilidad normal
# Curvas
curve(pnorm(x, 0, sqrt(0.2)), -5, 5, col = "red", las = 1, ann = F, 
      xaxp = c(-5, 5, 10))
curve(pnorm(x),add = TRUE, col = "green")
curve(pnorm(x, 0, sqrt(5)), add = TRUE, col = "blue")
curve(pnorm(x, -2, sqrt(0.5)), add = TRUE, col = "pink")
# Leyenda
l <- expression(paste(mu, "= 0, ", sigma^2, "= 0.2"), paste(mu, "= 0, ", sigma^2, "= 1.0"), paste(mu, "= 0, ", sigma^2, "= 5.0"), paste(mu, "=-2, ", sigma^2, "= 0.5"))
legend("bottomright", legend = l, lty = c(1, 1, 1, 1), bty = "n", col = c("red", "green", "blue", "pink"), inset = .05, y.intersp = .75)
grid()

# Funci?n de fiabilidad normal R(t) = 1-F(t)
# Curvas
curve(1-pnorm(x, 0, sqrt(0.2)), -5, 5, col = "red", las = 1, ann = F, 
      xaxp = c(-5, 5, 10))
curve(1-pnorm(x),add = TRUE, col = "green")
curve(1-pnorm(x, 0, sqrt(5)), add = TRUE, col = "blue")
curve(1-pnorm(x, -2, sqrt(0.5)), add = TRUE, col = "pink")
# Leyenda
l <- expression(paste(mu, "= 0, ", sigma^2, "= 0.2"), paste(mu, "= 0, ", sigma^2, "= 1.0"), paste(mu, "= 0, ", sigma^2, "= 5.0"), paste(mu, "=-2, ", sigma^2, "= 0.5"))
legend("bottomright", legend = l, lty = c(1, 1, 1, 1), bty = "n", col = c("red", "green", "blue", "pink"), inset = .05, y.intersp = .75)
grid()

# Funci?n de riesgo normal z(t) = f(t)/R(t)
# Curvas




   
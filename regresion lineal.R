file = "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"

datos = read.table(file, header=TRUE)
head(datos)
View(datos)

library(ggplot2)

ggplot(datos, aes(Edad, Resistencia)) + 
  geom_point() + theme_light()

mod1 = lm(Resistencia ~ Edad, datos)
mod1

summary(mod1)


ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light()

names(mod1)

a=mod1$coefficients
a

dim(a)

mod1$fitted.values

mod1$residuals

datos$predicciones = predict(mod1)
datos$predicciones

View(datos)

ggplot(datos, aes(x=Edad, y=Resistencia)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend=Edad, yend=predicciones), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col='red') +
  theme_light()


b = cbind(datos$Resistencia, mod1$fitted.values, mod1$residuals)
View(b)



## Modelo sin el intercepto.

mod2 = lm(Resistencia ~ Edad-1, datos)
summary(mod2)



## Modelo en logaritmos.
y = log(datos$Resistencia)
x = log(datos$Edad)

mod3 = lm(y ~ x)
summary(mod3)

y1 = log(datos$Resistencia)
x1 = log(datos$Edad)
yg = mod3$fitted.values

datos3 = data.frame(x1,y1,yg)
View(datos3)

ggplot(datos3, aes(x=x1, y= y1)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend = x1, yend = yg), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y = yg), col='red') +
  theme_light()

?mean()    #What is mean()

getwd()    #Working directory

setwd()    #Change the working directory


##Coertion.

as.integer(1.233)   # 1

as.numeric(TRUE)   # 1
as.integer(TRUE)    # 1
as.character(456)   # '456'

class(1)    #Numeric
class(2.4)    #Numeric
class(FALSE)   #Logical


is.numeric('Hola')   #FALSE


## Operators.

78%%4    #Mod 

TRUE | FALSE    #TRUE
FALSE & FALSE    #FALSE
!TRUE    #FALSE


# Vectors.

vector = c(1:5)

length(vector)

vector + 1
vector - 5



## Data frames.

x = c(1, 2, 3)
y = c('Dog', 'Cat', 'Bird')

z = data.frame(x,y)



# Matrixes.

m = matrix(1:9, nrow = 3, ncol = 3)

t(m)    #Transpose


a = 1:5

rbind(a, a, a)
cbind(a, a, a)

dim(rbind(a, a, a))



#Functions.

myFunction = function(a, b){
  (a**b)/3
}

myFunction(1,2)



## Loops

for(i in 0:5){
  print(i**2)
}



for(i in 5:0){
  print(sqrt(i))
}



alumnos = c('Juana', 'Pedro', 'Luisa', 'Sara')

for(i in 1:length(alumnos)){
  print(paste('Hola,', alumnos[i]))
}



a = rnorm(5)
b = rnorm(5)
c = rnorm(5)

df = data.frame(a, b, c)

ncol(df)

df[1,]    #First row
df[,1]    #First column

sum(df[,1])
median(df[,1])

for(i in 1:length(df)){
  mediana = median(df[,i])
  print(mediana)
}

x = 1
y = 2

if(x > y){
  print('HolaMundo')
} else{
  print('ChaoMundo')
}

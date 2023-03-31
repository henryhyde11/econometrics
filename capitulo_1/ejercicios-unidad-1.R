## Ejercicio unidad 1.
## Ejercicio 1.

y = c(1,1,-1,0,-1)
x2 = c(1,0,1,0,1)
x3 = c(2,-1,0,-1,2)


# a.

x = matrix(c(rep(1, length(y)),x2 ,x3), ncol = 3)
x

xtx = t(x)%*%x            
xtx

xtx_inv = solve(xtx)
xtx_inv

xty = t(x)%*%y
xty

b = xtx_inv%*%xty
b


# b.
n = length(y)
k = 3

e = y - x%*%b
e

s2= (t(e)%*%e) / (n - k)
s2[1]

covb = s2[1] * xtx_inv
covb


# c.

sct = t(y)%*%y
scr = t(e)%*%e

r2 = (sct - scr) / sct
r2[1]


# d.
round(sum(e))

round(t(x)%*%e)


# e.

li = ((n-k)*s2)  / qchisq(0.975, n-k)
li

ls = ((n-k)*s2) / qchisq(0.025, n-k)
ls



## Ejercicio 7.

b2 = matrix(c(c(1,-1.5), c(-1.5,2.5)), ncol = 2)
b2

b3 = c(16,9)
b3

b2%*%b3

b1 = 4 - (2.5*3) + (1.5*5)
b1



covb = ((5.2)**2/6)*b2
covb



## Ejercicio 8.

xtx = matrix(c(c(9,-1), c(-1,1)), ncol = 2)
xtx

xtxy = c(-14,7)
xtxy

xtx_inv = solve(xtx)
xtx_inv

b = xtx_inv%*%xtxy
b

ym = 5/20
xm = 100/20
zm = 24/20

a = ym - (b[1,1]*xm) - (b[2,1]*zm)
a


B = rbind(a,b[1,1],b[2,1])
B


## Ejercicio 13.

xtx = matrix(c(c(50,0,100),c(0,100,-400),c(100,-400,200)),
                   ncol = 3)
xtx

xty = c(100,-200,300)
xty

xtx_inv = solve(xtx)
xtx_inv

be = xtx_inv%*%xty
be

ym = 1
x2m = 4
x3m = -2
x4m = 1

b1 = ym - (be[1,1]*x2m) - (be[2,1]*x3m) - (be[3,1]*x4m)
b1


b = rbind(b1, be)
b

yty = 400

ete = yty - (2*t(be)%*%xty) + (t(be)%*%xtx%*%be)
ete[1]

s2u = ete / 197 
s2u

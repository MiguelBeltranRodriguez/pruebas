# z = 30x+20y

# x+4y<=100
# 2x+y <=60
# x+y <=20

# y>0
# x>0

z<-(c(30,20))
a<-matrix(c(1,2,1,4,1,1), ncol=2)
b<-(c(100,60,20))
r<-rep('<=',3)

solucion<-solveLP(cvec = z,bvec = b,Amat = a, maximum=TRUE, const.dir =r,zero = 1e-9, tol = 1e-8,verbose = 4)
summary(solucion)


# z = 30x+20y

# x+4y>=100
# 2x+y>=60
# x+y >=20

# y>0
# x>0

z<-(c(30,20))
a<-matrix(c(1,2,1,4,1,1), ncol=2)
b<-(c(100,60,20))
r<-rep('>=',3)

solucion<-solveLP(cvec = z,bvec = b,Amat = a, maximum=FALSE, const.dir =r,zero = 1e-9, tol = 1e-8,verbose = 4)
summary(solucion)
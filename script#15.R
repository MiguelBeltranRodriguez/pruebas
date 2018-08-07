#Punto-a
f <- function(x) return(5*x-(exp(1)^x)-1)


#Punto-b

plot(f,from=-3,to=3)
abline(h=0,col="blue");
#raices: 0.5448804 y 2.396139


#Punto-c
g <- function(x) return((1+(exp(1)^x))/5)
#intervalo de [0,2] y [2,3]


#Punto-d

puntofijo <-function(g, x0){
  maxIter=100
  tol=1e-8
  k = 1
  repeat{
    x1 = g(x0)
    
    dx = abs(x1-x0)
    print(dx)
    x0 = x1
    cat("x_", k, "= ", x1, "\n")
    k = k+1
    #until
    if(dx< tol|| k > maxIter) break;
  }
  
  if( dx > tol ){
    cat("No hubo convergencia ")
  } else{
    cat("x* es aproximadamente ", x1, " con error menor que ", tol, "\n")
  }
  cat("Error de truncamiento", abs(dx-tol))
}

puntofijo(g,0)
puntofijo(g,2.5)
#Solo converge con la raiz de 0.5448804


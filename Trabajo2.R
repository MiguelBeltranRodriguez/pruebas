#Tercer punto
tercerpunto<-function(n,e,x){
  
  y<- 1/2 * (x+n/x);
  print("Convergencia:");
  
  erroanter=abs(x-y);
  errornuevo=0;
  contador=0;
  plot(erroanter,type="s", main="plot(x,type=\"s\")");
  while (abs(x-y) > e) {
    
    errornuevo = abs(x-y);
    
    print(errornuevo/erroanter);
    x<-y;
    y<- 1/2 *(x+(n/x));
    erroanter = errornuevo;
    contador= contador+1;
    text(contador,erroanter,".",cex=1.0, col="red" );
  }
  return(y);
} 

raiz=tercerpunto(7,1.E-8,12);


validez = raiz*raiz;

print("Resultado:");
format(round(raiz, 8), nsmall=8);
print("Validez:");
validez

#--------------------------------------------------------------
#Sexto punto

sextopunto<-function(n){
  while (n>0){
    d<- n%%2;
    n<- trunc(n/2);
    print(d);
    #print(n);
  }
}
sextopunto(5);
plot(funcionT, from=0, to=100);

funcionT<-function(n){
  
  return(log2(n)+1);
}

#T(n)= T(n/2)+1
#    =(T(n/4)+1)+1
#    = T(n/4)+2
#    =(T(n/8)+1)+2
#    = T(n/8)+3
#    ...
#    = T(n/(2^i))+i
#    ...
#    =T(n/(2^(log(n))))+log 2(n)
#    =T(1)+log2(n)
#Por tanto, T(n) es O(log n)

#---------------------------------------------------------------
#Septimo punto
# Remueve todos los objetos creados
rm(list=ls())
# Halla la raiz de Fx
Fx <- function(x) exp(-x) + x -2
F1x <- function(x) 1-exp(-x)

newton <- function(x) {
  for(i in 1:20) {
    x<-x-Fx(x)/F1x(x)
    if (Fx(x) == 0) break
    error<-abs(Fx(x)/F1x(x))
    cat("X=",x,"\t","E=",error,"\n")
  }
}
newton(1)




#Metodo #1

volbiseccion<-function(largo,ancho){
  
  a = 0;
  b = largo;
  x = ((a+b)/2);
  
  i = 0;

  error = abs(volumen(largo,ancho,x)-1000);
  while(error>1.E-8){
    
    
    print(x);
    i = i +1;
    if(volumen(largo,ancho,x)<1000)
    {
      b=x;
    }
    else
    {
      a=x;
    }
    
    
    x <- ((a+b)/2);
    error = abs(volumen(largo,ancho,x)-1000);
  }
  
  
  return (x);
  
}
volumen<-function(largo, ancho, x){
  return((largo-2*x)*(ancho-2*x)*x);
}

cubica<-function(largo, ancho){
 
  
  return()
}

y = volbiseccion(32,24);
y




#Metodo #2
Fx <- function(x, largo, ancho) {return ((largo-2*x)*(ancho-2*x)*x)}
F1x <- function(x, largo, ancho) {return (12*x^2-4*ancho*x-4*largo*x+largo*ancho)}

newton <- function(x, largo, ancho) {
  for(i in 1:60) {
    x<-x-Fx(x, largo, ancho)/F1x(x, largo, ancho)
    if (Fx(x, largo, ancho) == 1000) break
    error<-abs(Fx(x, largo, ancho)/F1x(x, largo, ancho))
    cat("X=",x,"\t","E=",error,"\n")
  }
}
newton(10, 32, 24)



sqrt()


#1. La etapa de verificación
#2. Cálculo básico y pensamiento algoritmico, ademas de conocimientos basicos en el area al que pertenece el modelo matematico.
#3. La eficiencia es el mayor problema, ademas el margen de error puede ser mayor
#4. El error de truncamiento puede llegar a ser mucho mas objetivo que el de redondeo
#5. La velocidad y la eficacia
#6. Para comprobar si el algoritmo si es eficaz









#PUNTO 12: Funcion para hallar la raiz en R: uniroot() en el cual utiliza un metodo de prueba y error para irse acercando a 0

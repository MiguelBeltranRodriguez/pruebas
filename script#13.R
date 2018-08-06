

Fx <- function(x, b) {return (x*x-b)}
F1x<- function(x, b){return(2*x)}
  

newton <- function(x , b) {
    
  for(i in 1:10) {
    x<-x-Fx(x, b)/F1x(x, b)
    if (Fx(x, b) == sqrt(b)) break
    error<-abs(Fx(x, b)/F1x(x, b))
    cat("X=",x,"\t","E=",error,"\n")
  }
}
newton(2, 3)
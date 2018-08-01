#12, 1a, 7, 13 ------
#2,3 o 4 o 5, 6, 15

hornet<-function(p,n,x ,iter){
  y = p[c(1)];
  i = 0;
  for(i in 1:n+1){
    y = x*y + p[c(i)];
    
  }
  print(iter+i*2);
  return(y);
}

eval<-function(p, n, x ,iter){
  s = 0;
  i = 0;
  for(i in 0:n){
    s = s +(p[c(i+1)]*(x^(n-i)));
    print(i+1);
  }
  print(iter+i*3);
  return(s);
}

derivar<-function(p, n){
  d=numeric(0);
  iter = 0;
  for(i in 0: n){
    
    d[c(i+1)] = (n-i)*p[c(i+1)];
    iter = iter+1;
  }
  x = hornet(d, n-1, -2, iter);
  return(x)
}

p<-c(2,0,-3,3,4)

resultado = derivar(p,4);
resultado

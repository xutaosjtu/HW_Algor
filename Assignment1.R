swap <-function(x){
  if(leng(x)==1){
    return(x)
  }
  if(x[1]>x[2]){
    min = x[2];x[2]=x[1]; x[1] = min;
    return(x)
  }
  else return(x)
}

combine <- function(x,y){
  z = NULL
  k=1;j=1;
  tmp1 = x[k]
  tmp2 = y[j]
  while(k<=length(x) & j<=length(y)){
    if(tmp1<tmp2){
      z = c(z,tmp1)
      k = k +1
      tmp1 = x[k]
    }
    else{
      z = c(z,tmp2)
      j = j+1
      tmp2 = y[j]
    }
  }
  
  if(k>length(x)){
    return(z = c(z,y[j:length(y)]))
  }
  else return(z = c(z,x[k:length(x)]))
  
}

inverse<- function(A, n){
  if(n==1){
    #print("A = ");
    #print(A)
    return(A);
  }
  else{
    new_n = round(n/2)
    x = inverse(A[1:new_n],new_n)
    y = inverse(A[(new_n+1):n],(n-new_n))
    z = combine(x,y)
    return(z)
  }
}


f = function(x){
  return(c(x*x,x+2))
}

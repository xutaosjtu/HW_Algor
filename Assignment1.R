#swap <-function(x){
#  if(leng(x)==1){
#    return(x)
#  }
#  if(x[1]>x[2]){
#    min = x[2];x[2]=x[1]; x[1] = min;
#    return(x)
#  }
#  else return(x)
#}

combine <- function(x,y){
  count = 0
  z = NULL
  i=1;j=1;
  #tmp1 = x[k]
  #tmp2 = y[j]
  while(i<=length(x) & j<=length(y)){
    if(x[i]<y[j]){
      z = c(z,x[i])
      i = i +1
    }
    else{
      z = c(z,y[j])
      j = j+1
      count = count + length(x)-i+1
    }
  }
  #print(count)
  if(i>length(x)){
    z = c(z,y[j:length(y)])
  }
  else z = c(z,x[i:length(x)])
  
  return(list(count = count, z= z))
}

inverse<- function(A, n){
  if(n==1){
    return(list(count = 0, z = A));
  }
  else{
    new_n = round(n/2)
    x = inverse(A[1:new_n],new_n)
    y = inverse(A[(new_n+1):n],(n-new_n))
    z = combine(x[[2]],y[[2]])
    z[[1]] = z[[1]]+x[[1]]+y[[1]]
    return(z)
  }
}

count=0
inverse(a,6)

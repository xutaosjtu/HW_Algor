# The goal of this problem is to implement a variant of the 2-SUM algorithm (covered in the Week 6 lecture on hash table applications).
# The file contains 1 million integers, both positive and negative (there might be some repetitions!).This is your array of integers, with the ith row of the file specifying the ith entry of the array.
# 
# Your task is to compute the number of target values t in the interval [-10000,10000] (inclusive) such that there are distinct numbers x,y in the input file that satisfy x+y=t. (NOTE: ensuring distinctness requires a one-line addition to the algorithm from lecture.)
# 
# Write your numeric answer (an integer between 0 and 20001) in the space provided.

data = scan("algo1-programming_prob-2sum.txt")
data = sort(data)

tree=NULL
for(i in 1:length(data)){
  if(i==1) tree[1]=data[i]
  else {
    if(data[i]> tree[i-1])
  }
}

th.vector <- function(perm){
  comp<-function(a, b){
    if(a==b) return(0)
    if (a>b) return (1)
    return(2)
  }
  tree.add<-function(value){
    if(max.idx == 0){
      max.idx <<-1
      values[max.idx]<<-value
    }
    else{
      cur.idx <- 1
      while(TRUE){
        cr<-comp(value, values[cur.idx])
        if(cr==0) return()
        next.idx<- idxs[cur.idx, cr]
        if(next.idx == 0){
          max.idx<<-max.idx+1
          idxs[cur.idx, cr] <<- max.idx
          values[max.idx]<<-value
          return()
        }
        else{
          cur.idx <- next.idx
        }
      }
    }
  }
  max.idx <- 0
  values <- numeric(length(perm))
  idxs<-matrix(0, nrow =length(perm), ncol = 2, dimnames = list(NULL, c("high", 'low')))
  
  tmp = sapply(perm, function(x) tree.add(x))

}


upper.range = sapply(data, function(x) x+max(data))

rst = sapply(data, function(x) which(data==(t-x)))

h = hash(as.character(data), data)

a = Sys.time()
rst = has.key(tmp, h)
b = Sys.time()
b-a

rst=NULL
for(i in 1:1000){
  a = Sys.time()
  has.key(as.character(data[1:i]), h)
  b = Sys.time()
  rst = c(rst,b-a)
}

t = -10000:10000
sapply(t, 
       
       )

twosum<-function(x, data, data.hash){
  rst = sapply(data, function(y) data[as.character(x-y)])
  return(rst)
}

t = -10000:10000
tmp=sapply(t, function(x) sum(x-2*data))

a = Sys.time()
rst = sapply(t, function(x) return(sum((x-data) %in% data)))
b = Sys.time()
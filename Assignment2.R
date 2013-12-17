swap<-function(x,y){
  tmp = x
  x = y
  y = x
}

partition <- function(A, l, r){
  pivot = A[l];
  i =l+1
  for(j in (l+1):r){
    if (A[j] < pivot) {
      A[c(i,j)] = swap(A[i], A[j])
      i = i+1
    }
  }
  A[c(l,i-1)]swap(A[l], A[i-1])
  return(list(A, i))
}

ChoosePivot<-function(A, n){
  tmp.i = c(1, ceiling(n/2), n)
  tmp = A[tmp.i]
  i = which(tmp == median(tmp))[1]
  return(list(tmp[i], tmp.i[i]))
}

ChoosePivot<-function(A, n){
  return(list(A[n], n))
}

QuickSort<- function(A){
  n = length(A)
  
  if(n <=1) return(list(A, 0))
  
  p.comp = ChoosePivot(A, n)
  p = p.comp[[1]]
  if(p.comp[[2]] !=1) A[c(1,p.comp[[2]])] = swap(A[1], A[p.comp[[2]]])
  
  l = 1
  r = length(A)
  A.comp = partition(A, l ,r)
  A = A.comp[[1]]
  #print(A)
  l = A.comp[[2]]
  #print(l)
  count = n-1;
  count1 = 0; count2 = 0
  part1 = list();  part2 = list()
  if(l >2){
    part1 = QuickSort(A[1:(l-2)])
    A[1:(l-2)] = part1[[1]]
    count1 = part1[[2]]
  }
  if(l<n){
    part2 = QuickSort(A[l:n])
    A[l:n] = part2[[1]]
    count2 = part2[[2]]
  }
  count = count + count1 + count2
  #print(count)
  return(list(A, count))
}

# The file contains the adjacency list representation of a simple undirected graph. There are 200 vertices labeled 1 to 200. The first column in the file represents the vertex label, and the particular row (other entries except the first column) tells all the vertices that the vertex is adjacent to. So for example, the 6th row looks like : "6  155	56	52	120	......". This just means that the vertex with label 6 is adjacent to (i.e., shares an edge with) the vertices with labels 155,56,52,120,......,etc
# 
# Your task is to code up and run the randomized contraction algorithm for the min cut problem and use it on the above graph to compute the min cut. (HINT: Note that you'll have to figure out an implementation of edge contractions. Initially, you might want to do this naively, creating a new graph from the old every time there's an edge contraction. But you should also think about more efficient implementations.) (WARNING: As per the video lectures, please make sure to run the algorithm many times with different random seeds, and remember the smallest cut that you ever find.) Write your numeric answer in the space provided. So e.g., if your answer is 5, just type 5 in the space provided.

f = scan("D:/Users/tao.xu/Documents/Online courses/Algorithm/kargerMinCut.txt", sep = "\n", what = character())
data = strsplit(f, split = "\t")
which(lapply(data, anyDuplicated)!=0)
data = lapply(data, function(x) as.numeric(x[2:length(x)]))



edgeContraction <- function(data, u, v){
  ## remove u and replace it with v
  tmp = data[[u]]
  data[[v]] = c(data[[v]], tmp) ## move all links of node u to node v, except the link between v and u which is a self loop
  data = data[-u]## remove node u
  data=lapply(data, 
              function(x){
                x[which(x==u)]=v
                tmp = x[which(x>u)]
                x[which(x>u)] = tmp-1
                return(x)
                }
              )
  for(i in 1:length(data)){
    data[[i]] = data[[i]][data[[i]]!=i]
  }
  return(data)
}

randomizedContraction <- function(data, seed){
  i = 1
  while(length(data)>3){
    set.seed(seed)
    u = sample(1:length(data),1)
    #print(u)
    set.seed(seed)
    v = sample(data[[u]],1)
    #print(v)
    #print(length(data))
    data = edgeContraction(data, u, v)
    i = i+1
  }
  return(data)
}

rst = NULL
for(i in 1:100){
  tmp = randomizedContraction(data, i)
  rst = c(rst, length(tmp[[1]]))
}

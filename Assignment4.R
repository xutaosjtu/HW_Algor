# The file contains the edges of a directed graph. Vertices are labeled as positive integers from 1 to 875714. Every row indicates an edge, the vertex label in first column is the tail and the vertex label in second column is the head (recall the graph is directed, and the edges are directed from the first column vertex to the second column vertex). So for example, the 11th row looks liks : "2 47646". This just means that the vertex with label 2 has an outgoing edge to the vertex with label 47646
# 
# Your task is to code up the algorithm from the video lectures for computing strongly connected components (SCCs), and to run this algorithm on the given graph. 
# 
# Output Format: You should output the sizes of the 5 largest SCCs in the given graph, in decreasing order of sizes, separated by commas (avoid any spaces). So if your algorithm computes the sizes of the five largest SCCs to be 500, 400, 300, 200 and 100, then your answer should be "500,400,300,200,100". If your algorithm finds less than 5 SCCs, then write 0 for the remaining terms. Thus, if your algorithm computes only 3 SCCs whose sizes are 400, 300, and 100, then your answer should be "400,300,100,0,0".
# 
# WARNING: This is the most challenging programming assignment of the course. Because of the size of the graph you may have to manage memory carefully. The best way to do this depends on your programming language and environment, and we strongly suggest that you exchange tips for doing this on the discussion forums.

nodes = data.frame(id=c(1:10), state= rbinom(10, 1,0.3))

DFS<-function(graph, i){
  nodes[i,2]<<-1
  leader[i]<<- s
  for (j in graph[which(graph[,1]==i),2]){
    if(nodes[j,2]!=1) {
      #print(j)
      DFS(graph, j)
    }
  }
  t<<-t+1
  f[i] <<- t
}


DFS_loop<-function(graph){
  for(i in nrow(nodes):1){
    if(nodes[i,2]==0){
      s <<- i
      DFS(graph, i)
    }
  }
  return(data.frame(f, leader))
}

g_rev = graph[,2:1]
f=rep(0,max(graph))
leader=rep(0,max(graph))
t = 0;
s=0
nodes = cbind(1:max(graph), rep(0,max(graph)))
tmp = DFS_loop(g_rev)

g_new = apply(graph, 2, function(x) f[x])
f=rep(0,max(graph))
leader=rep(0,max(graph))
t = 0;
s=0
nodes = cbind(1:max(graph), rep(0,max(graph)))
tmp2 = DFS_loop(g_new)













dijkstra<-function(graph,initial_node)
{
unvisited_set<-unique(graph$v1) #in the graphic is like puting it to white/red

distance<-vector(length=6) #this is lil blue number in the visual one
distance[initial_node]<-0
distance[-initial_node]<-9999

while (length(unvisited_set)>0)
{

  
current<-unvisited_set[1] 
x<-graph[graph$v1==current,]

for (i in 1:nrow(x))
  {
      if  ((distance[current]+x$w[i]) < distance[x$v2[i]])
      {distance[x$v2[i]]<-(distance[current]+x$w[i])}
}

unvisited_set<-unvisited_set[-1]
}
return(distance)
}










  
  
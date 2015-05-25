#setwd('D:/Postgraduate/Course/2-semester/R-language/HiveR')

p =c( 'kenzi','taylor','kenzid','aero','boy','girl')
axis =c( 1,1,1,1,2,2)
r =c( 1,2,3,4,1,2)
size=c(1,1.2,1.5,2.0,1,1.2)
nodes = data.frame(id= c(1:6),lab = p,axis= axis,radius = r, size = size,color = rep('purple',6))
nodes$id <- as.integer(nodes$id)
nodes$axis <- as.integer(nodes$axis)
nodes$lab <- as.character(nodes$lab)
nodes$color <- as.character(nodes$color)
nodes$radius <- as.numeric(nodes$radius)
nodes$size <- as.numeric(nodes$size)
id1 = c(1,2,3,4)
id2 = c(5,5,5,6)
weight = c(1,2,3,4)
color = c('pink','pink','pink','lightblue')
edges = data.frame(id1= id1,id2=id2,weight=weight,color =color)
edges$id1 <- as.integer(edges$id1)
edges$id2 <- as.integer(edges$id2)
edges$color <- as.character(edges$color)
edges$weight <- as.numeric(edges$weight)
PS <- list()
PS$nodes <- nodes
PS$edges <- edges
PS$type <- "2D"
PS$desc <- "a little test"
PS$axis.cols <- c("grey", "grey")
class(PS) <- "HivePlotData"

chkHPD(PS) # answer of FALSE means there are no problems
sumHPD(PS)
plotHive(PS, ch = 0.1, bkgnd = "white")

library(igraph)
library(tidyr)
library(plyr)
library(reshape2)

data <- read.csv('/home/saintlyvi/Documents/ERC/mgmt/FEViz/FutureEnergyContacts2017-01-23.csv', stringsAsFactors = FALSE)
data$RI <- strsplit(data$Research.Interests, split = ",")
data$TS <- strsplit(data$Technical.Skills, split = ",")

TS <- data %>% unnest(TS)
bigTS <- table(TS$Name, TS$TS)
adjTS <- crossprod(t(bigTS))

RI <- data %>% unnest(RI)
bigRI <- table(RI$Name, RI$RI)
interests <- colnames(bigRI)
researchers <- rownames(bigRI)

#create edgelist (relationships between researchers)
links <- data.frame(n1=factor(levels=researchers), n2=factor(levels=researchers), type=factor(levels=interests))
n <- 1
for (r in 1:dim(bigRI)[1]) {
    for (s in 1:dim(bigRI)[1]) {
        if (r == s) {
            next
        }
        linkcount <- which(bigRI[r,]==1 & bigRI[s,]==1)
        if (length(linkcount) == 0) {
            next
            }
        else if (length(linkcount) == 1) {
            links[n,1] <- researchers[r]
            links[n,2] <- researchers[s]
            links[n,3] <- interests[linkcount]
            n <- n+1
            }
        else {
            for (l in 1:length(linkcount)){
                links[n,1] <- researchers[r]
                links[n,2] <- researchers[s]
                links[n,3] <- interests[linkcount[l]]
                n <- n + 1
            }
        }
    }
}
links$weight <- 1 #add link weight of 1
weighted_links <- dcast(links, n1 + n2 ~ weight, sum) #removes RI information and weighs by number of connections
names(weighted_links)[3] <- 'weight'

#Create nodes
groups <- unique(data$Research.Group)
nodes <- data[,c('Name','Research.Group')]
nodes$Group.Type <- sapply(nodes$Research.Group, FUN = function(x) match(x, groups))
nodes$RI.Connect <- sapply(nodes$Name, FUN = function(y) count(links$n1)$freq[count(links$n1)$x==y]) #create node weight based on number of research interest connections

#Create the graph
net <- graph_from_data_frame(d=weighted_links, vertices=nodes, directed=F) 
simplenet <- simplify(net, remove.multiple = T, edge.attr.comb = list(weight='sum','ignore'))

#Prettify the graph
colrs <- c('#4daf4a', '#f781bf','#e41a1c','#984ea3','#ff7f00','#ffff33','#a65628','#377eb8')
V(net)$color <- colrs[V(net)$Group.Type]
V(net)$label.color <- "black"
V(net)$size <- V(net)$RI.Connect

#reposition vertex labels
#radian.rescale <- function(x, start=0, direction=1) {
#    c.rotate <- function(x) (x + start) %% (2 * pi) * direction
#    c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
#}
#lab.locs <- radian.rescale(x=1:nrow(nodes), direction=-1, start=0)

edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
E(net)$edge.color <- "gray80"
E(net)$width <- E(net)$weight^1.7

l <- layout_in_circle(net)

plot(net, edge.color=edge.col, edge.curved=.1, layout=l)  
legend(x=-1.5, y=-1.1, groups, pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

adjRI <- crossprod(t(bigRI)) #create adjacency matrix
adjRI[lower.tri(adjRI, diag = TRUE)] <- 0 #only keep top triangle of adjacency matrix (not yet sure why)
g <- graph.adjacency(adjRI, mode = 'undirected') #create igraph object from adjacency matrix (with edges and nodes)


#http://stackoverflow.com/questions/9850514/simple-conversion-to-edgelist-with-r
#http://stackoverflow.com/questions/20072644/edges-from-matching-column-values-in-r
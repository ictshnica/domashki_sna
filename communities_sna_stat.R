library(readr)
drugattr <- read.csv("~/drugattr.csv", sep = ";")
drugnet <- read.csv("~/drugnet.csv", sep = ";")
drugnet$ID=NULL
drugnet=as.matrix(drugnet)
library(igraph)
ig <- graph.adjacency(drugnet, mode="undirected", weighted=TRUE)
deg=degree(ig)
clos=closeness(ig)
betw=betweenness(ig)
ID=c(1:293)
degt=data.frame(ID,deg,clos,betw)

degt=left_join(degt,drugattr)

fit <- aov(deg ~ Gender+Ethnicity, data=degt)
summary(fit)

fit2 <- aov(clos ~ Gender+Ethnicity, data=degt)
summary(fit2)

fit3 <- aov(betw ~ Gender+Ethnicity, data=degt)
summary(fit3)

linm = lm(deg~ as.factor(Gender)+as.factor(Ethnicity)+clos+betw, data=degt)
summary(linm)

linm2 = lm(clos~ as.factor(Gender)+as.factor(Ethnicity)+deg+betw, data=degt)
summary(linm2)

linm3 = lm(betw~ as.factor(Gender)+as.factor(Ethnicity)+deg, data=degt)
summary(linm3)

vif(linm3) # variance inflation factors 
sqrt(vif(linm3))

library(sjPlot)
sjt.lm(linm, linm3)

write.csv(degt,"degt.csv")

V(ig)$name = c(1:293)
drugattr$Ethnicity=as.factor(drugattr$Ethnicity)
V(ig)$Sex=as.character(drugattr$Gender[match(V(ig)$name,drugattr$ID)])
V(ig)$Eth=as.character(drugattr$Ethnicity[match(V(ig)$Eth,drugattr$ID)])
V(ig)$color=V(ig)$Eth #assign the "Sex" attribute as the vertex color
V(ig)$color=gsub("1","red",V(ig)$color) #Females will be red
V(ig)$color=gsub("2","blue",V(ig)$color)
V(ig)$color=gsub("3","green",V(ig)$color)
V(ig)$color=gsub("4","green",V(ig)$color)
V(ig)$color=gsub("5","green",V(ig)$color)
V(ig)$color=gsub("6","green",V(ig)$color)
V(ig)$color=gsub("7","green",V(ig)$color)
#Males will be blue
plot.igraph(ig,vertex.label=NA,layout=layout.fruchterman.reingold)
V(ig)$size=degree(ig)*2 #because 1 is a small size for a node, I'm just multiplying it by 5
plot.igraph(ig,vertex.label=NA,layout=layout.fruchterman.reingold)

V(ig)$Eth=as.character(drugattr$Ethnicity[match(V(ig)$Eth,drugattr$ID)])


pc=power_centrality(ig)
alpha_centrality(ig)

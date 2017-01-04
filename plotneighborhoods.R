library(rgeos)
library(maptools)
library(ggplot2)
library(ggmap)
library(scales)
library(RColorBrewer)

setwd("C:\\Users\\lucas\\Desktop\\DePaul\\CSC478\\Project\\ChiNeighborhood\\")
chinhsptl <- readShapeSpatial("geo_export_6a5bf684-6043-4830-bb7e-2bfd6002a9c8")

# plot(chinh)
# cents<-getSpPPolygonsLabptSlots(chinh)
# cx<-cents[,1]
# cy<-cents[,2]
# text(x=cx,y=cy,labels=chinh$community,cex=.5,font=c(2))

chinhsptl$fs=as.character(chinhsptl$community)

levels(chinhsptl$community)

nhclus <- read.csv("..\\comm_clus.csv")
nhclus$fs=as.character(nhclus$name)
levels(nhclus$name)



sptlclusnh<-data.frame('id'=chinhsptl$area_numbe,'fs'=as.character(chinhsptl$community))
assign<-merge(nhclus,sptlclusnh,by='fs')
assign$cluster<-as.factor(assign$cluster)

chinh<-fortify(chinhsptl,region='area_numbe')
mnhclus<-merge(chinh,assign,by='id',all.x=T)
mnhclus<-mnhclus[order(mnhclus$order),]

cents<-getSpPPolygonsLabptSlots(chinh)
nhLabel <- data.frame(x=cents[,1],y=cents[,2],nh=chinh$community)


ggplot() + theme_bw() + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        axis.line=element_blank(), axis.ticks=element_blank(), panel.border=element_blank(),
        axis.text=element_blank(), axis.title=element_blank(),
        legend.position=c(0.18,0.12), 
        legend.title=element_text(colour='black',size=16,face='bold'),
        legend.text=element_text(colour='black',size=10,face='bold')) +
  geom_polygon(data = mnhclus,
               aes(x = long, y = lat, group = group, fill = as.factor(cluster)), 
               color = "black", size = 0.25) +
  scale_fill_manual(name="Cluster",
                    labels=c("Medium Crime","High Property Crime",
                             "High Violent Crime","Medium Property Crime","Low Crime"),
                    values=c("yellow","purple","red","orange","green"),
                    guide=guide_legend(reverse=TRUE),drop=F) +
  coord_map()
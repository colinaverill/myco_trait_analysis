#Plotting global distribution of trait observations used in this analysis.
rm(list=ls())
source('paths.r')
library(ggplot2)
library(ggalt)
library(data.table)

#set output path.----
output.path <- sample_map_figure.path
ouput.path <- 'map.png'

#load data.----
d <- data.table(readRDS(intra_specific_analysis_data.path))
d <- d[,.(latitude, longitude)]
d <- d[complete.cases(d),]
lon <- d$longitude
lat <- d$latitude

#set output spec.----
png(filename=output.path,width=8,height=4.8,units='in',res=300)

world <- map_data('world')
map <- ggplot() + geom_cartogram(data = world, map = world, 
                                 aes(x=long, y = lat, group = group, map_id=region))
map <- map + coord_proj("+proj=wintri", ylim = c(-55,90))
map <- map + geom_point(aes(x = lon, y = lat), color = "yellow"    , size = .2)
map <- map + theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position="none",
                   panel.border=element_blank()
                  )
map

#end plot.----
dev.off()
#variance decomposition figure.
rm(list=ls())
source('paths.r')

#set output path.
output.path <- var_decomp_figure.path

#load data.----
d <- readRDS(variance_decomp_output.path)

#Get x axis labels.----
lab <- colnames(d)
lab <- substring(lab, 1, nchar(lab) - 4)
cat <- rownames(d)
cat.col <- gray.colors(length(cat))

#set output spec.----
png(filename=output.path,width=5,height=5,units='in',res=300)

#whole plot settings.----
par(oma = c(.2,1,0,1.9), mar = c(3.5,2,1,8))

#begin barplot.
BP <- barplot(as.matrix(d), las = 2, xaxt = 'n', 
              legend.text = T,
              args.legend=list(
                x=ncol(d) + 9,
                y=max(colSums(d))*0.75,
                bty = "n"))
#add x labels
ypos = -0.05
BP <- text(x = BP, y = ypos, srt = 45,
           adj = 1, labels = lab, xpd = TRUE)

#end plot.----
dev.off()
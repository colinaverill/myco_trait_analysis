#visualize N:P ratio responses.
rm(list=ls())
source('paths.r')
source('functions/p_report.r')
library(data.table)
library(caper)

library(MCMCglmm)

#output path.
output.path <- 'figures/trait_NP_plot.png'

#load data.----
d <- readRDS(pgls.glmm_NP_analysis.path)
log <- d$log10
non <- d$reg

#Based on senescent NP, seems like we should log transform. Green and root could go either way.
#residual comparison hased out below.
d <- log

#Get plotting names
names(d) <- c('Green N:P','Senescent N:P','Root N:P')

#setup to save.----
png(filename=output.path,width=9,height=4,units='in',res=300)

#Global plot settings.----
par(mfrow=c(1,3),
    mar = c(1.25,2,1.25,2),
    oma = c(1,4,1,1))
a.cex <- 1.2
trans.box <- c(1,0.8,0.6)
trans.box <- c(0.4,0.3,0.2)
limx <- c(0.8, 2.2)
box.lim <- c(limx[1], limx[1] + 1*(limx[2] - limx[1])/3, limx[1] + 2*(limx[2] - limx[1])/3, limx[2])
inc <- (box.lim[2] - box.lim[1])/2
lat.lab.pos <- c(box.lim[1] + inc,box.lim[2] + inc,box.lim[3] + inc)
x.inter.sp <- 0.08
x <- c(lat.lab.pos[1]-x.inter.sp,lat.lab.pos[1]+x.inter.sp,lat.lab.pos[2]-x.inter.sp,lat.lab.pos[2]+x.inter.sp,lat.lab.pos[3]-x.inter.sp,lat.lab.pos[3]+x.inter.sp)
box.shade.cols <- c('#f9bb41','#9dba32','#2abaac')
myc.col <- c('#f97de2','#9227af')
myc.col <- c('#00acd9','#cfe83c')

#plot loop!----
for(i in 1:length(d)){
  #data unique to model summary output.
  y <- d[[i]]$mean
  upr <- d[[i]]$upper
  lwr <- d[[i]]$lower
  trait.name <- names(d[i])
  lat.lab <- c('boreal','temperate','sub / tropical')
  lat.lab <- c('boreal','temperate','tropical')
  bore.N <- d[[i]]$sample_size[c(1,4)]
  temp.N <- d[[i]]$sample_size[c(2,5)]
  trop.N <- d[[i]]$sample_size[c(3,6)]
  N.lab <- c(paste(bore.N, collapse = ', '),
             paste(temp.N, collapse = ', '),
             paste(trop.N, collapse = ', '))
  N.lab <- paste0('N = (',N.lab,')')
  p.val <- d[[i]]$p.values
  p.lab <- unlist(lapply(p.val, p_report))
  
  #back log transform nutrient data.
    y <- 10^(y)
    upr <- 10^(upr)
    lwr <- 10^(lwr)      

  #y limits.
  limy = c(0, max(upr,na.rm = T)*1.05)
  
  #plot box.
  plot(y ~ x, pch = 16, xaxt='n', yaxt = 'n', cex = 0, ylim = limy, xlim = limx, ylab = NA, cex.axis = a.cex, bty='n', xaxs='i', yaxs='i')
  
  #y axis.
  z <- (limy[2] - limy[1])/3
  axis.ticks <- round(c(limy[1], limy[1] + z, limy[1] + 2*z, limy[2]), 1)
  axis(side = 2, at = axis.ticks, las = 2, col = NA, col.ticks = 'black')
  
  #drop rectangles.
  #if(i < 3)        {shade_col <- box.shade.cols[1]}
  #if(i > 2 & i < 6){shade_col <- box.shade.cols[2]}
  #if(i > 5)        {shade_col <- box.shade.cols[3]}
  shade_col <- box.shade.cols[2]
  rect(box.lim[1], limy[1], box.lim[2], limy[2], border = NA, col = adjustcolor(shade_col, trans.box[1]))
  rect(box.lim[2], limy[1], box.lim[3], limy[2], border = NA, col = adjustcolor(shade_col, trans.box[2]))
  rect(box.lim[3], limy[1], box.lim[4], limy[2], border = NA, col = adjustcolor(shade_col, trans.box[3]))
  
  #points and error bars.
  arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
  points(y ~ x, pch = 16, col = myc.col, cex = 2)
  
  #latitudinal zone text labels, sample size, significane labels.
  text(lat.lab, x = lat.lab.pos, y = limy[2] * 0.14)
  text(  p.lab, x = lat.lab.pos, y = limy[2] * 0.09)
  text(  N.lab, x = lat.lab.pos, y = limy[2] * 0.03)
  #trait label.
  mtext(trait.name, side = 3, adj = 0.02, line = 0.3)
  
  #legend in top right panel.
  #if(i == 2){
  #  #Drop empty plot to place legend in.----
  #  plot(y~x,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim = c(0,10), xlim = c(0,10))
  #  legend(0, 10,legend = c('arbuscular \nmycorrhizal','ectomycorrhizal'), 
  #         pch = 16, col = myc.col, bty = 'n', cex = 2,
  #         x.intersp = 0.7, y.intersp = 1.5)
  #}
  
  #unit label on y-axis
  if(i == 1){mtext(expression(paste('mg N (mg P)'^'-1')), side = 2, line = 3)}
 }
#end plot.----
dev.off()





#compare residuals: roots.
plot_resids <- F
if(plot_resids == T){
  par(mfrow = c(3,2))
  #compare residuals: green leaves.
  reg.green.fit <- predict(non$greenNP$model)
  reg.green.resid <- non$greenNP$data$greenNP - reg.green.fit
  log.green.fit <- predict(log$greenNP$model)
  log.green.resid <- log10(log$greenNP$data$greenNP) - log.green.fit
  
  plot(reg.green.resid ~ reg.green.fit)
  plot(log.green.resid ~ log.green.fit)
  
  
  #compare residuals: senescent leaves.
  reg.senes.fit <- predict(non$senesNP$model)
  reg.senes.resid <- non$senesNP$data$senesNP - reg.senes.fit
  log.senes.fit <- predict(log$senesNP$model)
  log.senes.resid <- log10(log$senesNP$data$senesNP) - log.senes.fit
  
  plot(reg.senes.resid ~ reg.senes.fit)
  plot(log.senes.resid ~ log.senes.fit)
  
  #compare residuals: roots.
  reg.roots.fit <- predict(non$rootsNP$model)
  reg.roots.resid <- non$rootsNP$data$rootsNP - reg.roots.fit
  log.roots.fit <- predict(log$rootsNP$model)
  log.roots.resid <- log10(log$rootsNP$data$rootsNP) - log.roots.fit
  
  plot(reg.roots.resid ~ reg.roots.fit)
  plot(log.roots.resid ~ log.roots.fit)
}

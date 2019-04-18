#phylogenetic trait correlation figure NOT WORKING YET.
#clear environment, load packages
rm(list=ls())
source('paths.r')
library(wesanderson)
library(scales)
library(caper)
library(data.table)

#set output path.----
out.path <- phylo_estimated_traits_figure.path

#load data.----
d <- readRDS(phylo_estimated_traits.path)

#load models, grab lambda values.----
z <- readRDS(phy_est_models_data.path)
lambda <- list()
for(i in 1:length(z)){
  lambda[[i]] <- z[[i]]$lambda
}
lambda <- unlist(lambda)
z <- data.frame(names(z), lambda)
colnames(z)[1] <- 'analysis'
z$analysis <- gsub('root.l','root.L',z$analysis)



#setup to save.----
png(filename=out.path,width=9,height=10,units='in',res=300)

#Global plot settings.----
#setup plot
par(mfrow = c(3,3),
    mar = c(2,2,2,2),
    oma = c(4.5,4.5,.2,.2))
#pick colors.
cols <- c('#00acd9','#cfe83c')
r.line.col <- '#ff4ce3'
#assign colors based on mycorrhizal status
d$col <- NA
d$col <- ifelse(d$MYCO_ASSO == 'ECM', cols[2], d$col)
d$col <- ifelse(d$MYCO_ASSO == 'AM' , cols[1], d$col)
#set transparency
trans <- 0.6
#set point size.
p.cex <- 0.8

#log leaf lifespan.----
trait <- 'log.LL'
label <- 'a. Leaf Lifespan'
units <- 'log(months)'
trait_estimate <- paste0(trait,'_estimate')
tr.form <- formula(paste0(trait,'~',trait_estimate))
mod<- lm(tr.form, data = d)
s.mod <- summary(mod)
plot(tr.form, data = d, cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)


#root lifespan.----
trait <- 'root.L'
label <- 'b. Root Lifespan'
units <- 'log(months)'
trait_estimate <- paste0(trait,'_estimate')
tr.form <- formula(paste0(trait,'~',trait_estimate))
mod<- lm(tr.form, data = d)
s.mod <- summary(mod)
plot(tr.form, data = d, cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)


#Drop empty plot to place legend in.----
plot(Ngreen ~ Pgreen, data = d,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim = c(0,10), xlim = c(0,10))
legend(0, 10,legend = c('arbuscular \nmycorrhizal','ectomycorrhizal'), pch = 16, col = c(cols[1],cols[2]), bty = 'n', x.intersp = 0.7, y.intersp = 1.5, cex = 2)

#Ngreen.----
trait <- 'Ngreen'
label <- 'c. Nitrogen Green'
units <- expression(paste('log(mg N (g tissue)'^'-1',')'))
trait_estimate <- paste0(trait,'_estimate')
tr.form <- formula(paste0(trait,'~',trait_estimate))
mod<- lm(tr.form, data = d)
s.mod <- summary(mod)
#Modified to exclude one very low N green value that throws the whole axis.
plot(tr.form, data = d[d$Ngreen > -1,], cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)

#Nsenes.----
trait <- 'Nsenes'
label <- 'd. Nitrogen Senescent'
units <- expression(paste('log(mg N (g tissue)'^'-1',')'))
mod<- lm(Nsenes ~ Nsenes_estimate, data = d)
s.mod <- summary(mod)
plot(d$Nsenes ~ d$Nsenes_estimate, cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)

#Nroots.----
trait <- 'Nroots'
label <- 'e. Nitrogen Roots'
units <- expression(paste('log(mg N (g tissue)'^'-1',')'))
trait_estimate <- paste0(trait,'_estimate')
tr.form <- formula(paste0(trait,'~',trait_estimate))
mod<- lm(tr.form, data = d)
s.mod <- summary(mod)
plot(tr.form, data = d, cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)

#Pgreen.----
trait <- 'Pgreen'
label <- 'f. Phosphorus Green'
units <- expression(paste('log(mg P (g tissue)'^'-1',')'))
trait_estimate <- paste0(trait,'_estimate')
tr.form <- formula(paste0(trait,'~',trait_estimate))
mod<- lm(tr.form, data = d)
s.mod <- summary(mod)
plot(tr.form, data = d, cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)

#Psenes.----
trait <- 'Pgreen'
label <- 'g. Phosphorus Senescent'
units <- expression(paste('log(mg P (g tissue)'^'-1',')'))
trait_estimate <- paste0(trait,'_estimate')
tr.form <- formula(paste0(trait,'~',trait_estimate))
mod<- lm(tr.form, data = d)
s.mod <- summary(mod)
plot(tr.form, data = d, cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)

#Proots.----
trait <- 'Proots'
label <- 'h. Phosphorus Roots'
units <- expression(paste('log(mg P (g tissue)'^'-1',')'))
trait_estimate <- paste0(trait,'_estimate')
tr.form <- formula(paste0(trait,'~',trait_estimate))
mod<- lm(tr.form, data = d)
s.mod <- summary(mod)
plot(tr.form, data = d, cex = p.cex, pch = 16, col = alpha(d$col, trans), xlab = NA, ylab = NA, cex.axis = 1.2)
line.col <- ifelse(s.mod$coefficients[2,4] < 0.05,r.line.col,'gray')
line.type <- ifelse(s.mod$coefficients[2,4] < 0.05, 1,2)
abline(mod, lwd = 2, col = line.col, lty = line.type)
lambda.val <- z[z$analysis == trait,c('lambda')]
lambda.lab <- bquote(lambda == .(format(lambda.val,digits=2)))
mtext(lambda.lab, side = 3, adj = 0.05, line = -7)
mtext(label, side = 3, adj = 0.05, line = -2)
mtext(units, side = 3, adj = 0.05, cex = 0.8, line = -3.5)
mtext(bquote(R^2 == .(round(s.mod$r.squared, 2))), side = 3, adj = 0.05, line = -5.25)

#outer labels.----
mtext('observed trait', side = 2, outer = T, cex = 2, line = 1.5)
mtext('phylogenetic estimated trait', side = 1, outer = T, cex = 2, line = 2.5)

#end plot.----
dev.off()

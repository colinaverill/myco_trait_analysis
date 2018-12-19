report_myco_pgls <- function(mod, dat){
  #grab predictors, subsetted data frame.
  preds <- mod$varNames
  mod.name <- preds[1]
  #get log10 stuff out if present.
  mod.name <- gsub('log10\\(','',mod.name)
  mod.name <- gsub('\\)','',mod.name)
  preds[1] <- mod.name
  dat <- as.data.frame(dat)
  dat <- dat[,preds]
  dat <- dat[complete.cases(dat),]
  
  #Get climate cmeans.
  mat <- mean(dat$mat.c)
  map <- mean(dat$map.c)
  
  #get AM and ECM counts
  n.AM <- nrow(dat[dat$MYCO_ASSO == 'AM' ,])
  n.EM <- nrow(dat[dat$MYCO_ASSO == 'ECM',])
  
  #get AM-ECM estimates.
  sum <- summary(mod)
  p.mat <- sum$coefficients[grep('mat.c',rownames(sum$coefficients)),'Estimate']
  p.map <- sum$coefficients[grep('map.c',rownames(sum$coefficients)),'Estimate']
  intercept <- sum$coefficients[grep('(Intercept)',rownames(sum$coefficients)),'Estimate']
  myco      <- sum$coefficients[grep('MYCO',rownames(sum$coefficients)),'Estimate']
  myco.err  <- sum$coefficients[grep('MYCO',rownames(sum$coefficients)),'Std. Error']
  myco.p    <- sum$coefficients[grep('MYCO',rownames(sum$coefficients)),'Pr(>|t|)']
  AM <- intercept + p.mat*mat + p.map*map
  EM <- AM + myco
  
  #get output and return.
  out <- c(mod.name,AM,EM,myco.err,myco.p,n.AM,n.EM)
  names(out) <- c('trait','AM','ECM','error','p_val','N_AM','N_ECM')
  return(out)
}
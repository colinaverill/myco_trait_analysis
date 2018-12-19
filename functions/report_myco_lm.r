report_myco_lm <- function(mod){
  #grab predictors, subsetted data frame.
  dat <- model.frame(mod)
  preds <- colnames(dat)
  mod.name <- preds[1]
  #strip out log10 stuff if present.
  mod.name <- gsub('log10\\(','',mod.name)
  mod.name <- gsub('\\)','',mod.name)
  preds[1] <- mod.name
 
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
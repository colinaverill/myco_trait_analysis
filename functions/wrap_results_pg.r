#wrap_results_pg.r
wrap_results_pg <- function(mod, dat){
  effects <- summary(mod)$coefficients[,1]
  error <- summary(mod)$coefficients[,2]
  pval <- summary(mod)$coefficients[,4]
  mod.name <- paste0(mod$namey,'.pg')
  vars <- mod$varNames
  dat <- as.data.frame(dat)
  dat <- dat[,colnames(dat) %in% vars]
  dat <- dat[complete.cases(dat),]
  n.AM <- nrow(dat[dat$MYCO_ASSO ==  'AM',])
  n.EM <- nrow(dat[dat$MYCO_ASSO == 'ECM',])
  n.angio <- nrow(dat[dat$pgf ==  'angio',])
  n.gymno <- nrow(dat[dat$pgf ==  'gymno',])
  #start calculating and naming.
  to_return <- c(mod.name,effects,error,pval,n.AM,n.EM,n.angio,n.gymno)
  nm <- c('Intercept',vars[2:length(vars)])
  nm_err <- paste0(nm,'_err')
  nm_p <- paste0(nm,'_p')
  names(to_return) <- c('model',nm,nm_err,nm_p,'n.AM','n.ECM','n.angio','n.gymno')
  return(to_return)
}

#wrap_results_lm.r
wrap_results_lm <- function(mod){
  effects <- summary(mod)$coefficients[,1]
  error <- summary(mod)$coefficients[,2]
  pval <- summary(mod)$coefficients[,4]
  mframe <- model.frame(mod)
  mod.name <- paste0(colnames(mframe)[1],'.lm')
  vars <- colnames(mframe)
  n.AM <- nrow(mframe[mframe$MYCO_ASSO ==  'AM',])
  n.EM <- nrow(mframe[mframe$MYCO_ASSO == 'ECM',])
  n.angio <- nrow(mframe[mframe$pgf ==  'angio',])
  n.gymno <- nrow(mframe[mframe$pgf ==  'gymno',])
  #start calculating and naming.
  to_return <- c(mod.name,effects,error,pval,n.AM,n.EM,n.angio,n.gymno)
  nm <- c('Intercept',vars[2:length(vars)])
  if('biome' %in% vars){
    nm <- names(effects)
    nm[1:2] <- c('Intercept','MYCO_ASSO')
  }
  nm_err <- paste0(nm,'_err')
  nm_p <- paste0(nm,'_p')
  to_return <- data.frame(t(to_return))
  colnames(to_return) <- c('model',nm,nm_err,nm_p,'n.AM','n.ECM','n.angio','n.gymno')
  to_return$model <- as.character(to_return$model)
  to_return[,2:ncol(to_return)] <- lapply(to_return[, 2:ncol(to_return)],function(x) as.numeric(levels(x))[x])
  return(to_return)
}

#takes an lm or pgls mdeol summary object.
#returns a dataframe with effects, standard errors and p-values
wrap_results <- function(model, data, trait, model_name){
  model_sum <- summary(model)
  #effects
  analysis <- model_name
  effects <- model_sum$coefficients[,1]
  errors <- model_sum$coefficients[,2]
  p.vals <- model_sum$coefficients[,4]
  
  #get counts. first figure out which predictors used.
  #depends on trait.myc.count function.
  preds <- rownames(model_sum$coefficients)
  counts <- trait.myc.count(eval(trait), data,
                            wood = ('wood' %in% preds),
                            gymno = ('gymno' %in% preds))
  #wrap it up.
  the_names <- c('analysis',
                 paste(names(effects),'_mu'  ,sep=''),
                 paste(names(errors) ,'_se'  ,sep=''),
                 paste(names(p.vals),'_p.val',sep=''),
                 'AM_n','EM_n')
  #change some names.
  the_names <- gsub('\\(Intercept)','AM', the_names)
  the_names <- gsub('MYCO_ASSOECM','EM', the_names)
  the_names <- gsub('woodinessW','wood',the_names)
  the_names <- gsub('pgfgymno.wood','gymno',the_names)
  the_names <- gsub('pgfherb','herb',the_names)
  the_names <- gsub('MYCO_ASSO','',the_names)
  
  to_return <- c(analysis,effects,errors,p.vals,counts$n[2:3])
  names(to_return) <- the_names
  to_return <- data.frame(to_return)
  to_return$merge.key <- rownames(to_return)
  colnames(to_return)[1] <- model_name
  
  #return output
  return(to_return)
}
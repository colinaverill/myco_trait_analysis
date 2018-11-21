#' pgls_step_aicc.r
#' step through all predictors, get aicc scores, remove predictor that generates best aicc improvement.
#' rinse and repeat until aicc no longer improves.
#' because mycorrhizal association is the focus of these analyses, it is excluded from aicc predictor removal.
#' If some combination of predictors breaks PGLS, it will keep running, and this will be reported in output.
#' depends on doParallel, caper, pic_pro() function.
#'
#' @param y          #dependent variable for PGLS.
#' @param x          #independent variables to select from for PGLS. (MYCO_ASSO default included.)
#' @param phylogeny  #phylogeny to use.
#' @param data       #data frame to use.
#'
#' @return
#' @export
#'
#' @examples
pgls_step_aicc <- function(y, x, phylogeny, data, n.cores = 1, log = F){
  #register parallel
  registerDoParallel(n.cores)
  
  #subset to complete cases.
  keep <- c(y,x,'Species','MYCO_ASSO')
  dat <- as.data.frame(data)
  dat <- dat[,keep]
  dat <- dat[complete.cases(dat),]
  
  #setoutput aicc output list.
  all.out <- list()
  
  #get reference case.
  if(log == F){
    mod <- pic_pro(y=y, x=c('MYCO_ASSO',x), phylogeny = phylogeny, trait.data = dat)
  }
  if(log == T){
    mod <- pic_pro(y=y, x=c('MYCO_ASSO',x), phylogeny = phylogeny, trait.data = dat, log = T)
  }
  to_return <- data.frame(paste(x, collapse = ','), mod$aicc, NA,1)
  colnames(to_return) <- c('preds','aicc','aicc.diff','aicc_round')
  all.out[[1]] <- to_return
  ref.aicc <- mod$aicc
  
  #run models leaving one predictor out at a time.
  j = 2
  ref.aicc_sub <- ref.aicc
  check = 1
  while(check > 0){
    round.out <- list()
    round.out <- 
    foreach(k = 1:length(x)) %dopar% {
      if(log == F){
        fit <- try(pic_pro(y, c('MYCO_ASSO',x[-k]), trait.data = dat, phylogeny = phylogeny), silent = T)
      }
      if(log == T){
        fit <- try(pic_pro(y, c('MYCO_ASSO',x[-k]), trait.data = dat, phylogeny = phylogeny, log = T), silent = T)
      }
      if(!(class(fit) == 'try-error')){
        to_return <- data.frame(paste(x[-k],collapse =','), fit$aicc)
        cat('try error in',y,'with',c('MYCO_ASSO',x[-k]),'as predictors. Continuing.') #may not print in foreach loop...
      }
      if(  class(fit) == 'try-error' ){
        to_return <- data.frame("try-error in pgls",NA)
      }
      colnames(to_return)  <- c('preds','aicc')
      return(to_return)
     }
    round.out <- do.call(rbind,round.out)
    round.out$aicc.diff <- c(ref.aicc_sub) - c(round.out$aicc)
    round.out$aicc_round <- rep(j, nrow(round.out))
    pos <- which.max(round.out$aicc.diff)
    check <- round.out$aicc.diff[pos]
    if(check > 0){
      x <- x[-pos]
    }
    all.out[[j]] <- round.out
    ref.aicc_sub <- round.out[pos,]$aicc
    j = j + 1
    cat(j-2,'rounds of aicc comparison complete.\n')
  }
  
  #summarize results.
  all.out <- do.call(rbind, all.out)
  final_return <- list(all.out,c('MYCO_ASSO',x))
  names(final_return) <- c('aicc_comparisons','win_preds')
  cat("best model includes",paste(c('MYCO_ASSO',x), collapse = ' , '),'as predictors.\n')
  return(final_return)
}
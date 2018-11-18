#' pgls_step_aicc.r
#' step through all predictors, get aicc scores, remove predictor that generates best aicc improvement.
#' rinse and repeat until aicc no longer improves.
#' because mycorrhizal association is the focus of these analyses, it is excluded from aicc predictor removal.
#' depends on doParallel.
#'
#' @param y 
#' @param x 
#' @param phylogeny 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
pgls_step_aicc <- function(y, x, phylogeny, data, n.cores = 1){
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
  mod <- pic_pro(y=y, x=c('MYCO_ASSO',x), phylogeny = phylogeny, trait.data = dat)
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
    foreach(i = 1:length(x)) %dopar% { 
      fit <- pic_pro(y, c('MYCO_ASSO',x[-i]), trait.data = dat, phylogeny = phylogeny)
      to_return <- data.frame(paste(x[-i],collapse =','), fit$aicc)
      round.out[[i]] <- to_return
    }
    round.out <- do.call(rbind,round.out)
    colnames(round.out)  <- c('preds','aicc')
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
  cat("best model includes",paste(c('MYCO_ASSO',x), collapse = ' , '),'as predictors.')
  return(final_return)
}
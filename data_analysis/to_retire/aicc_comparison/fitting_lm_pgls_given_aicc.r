#fit lm and pgls models based in aicc selection.
rm(list=ls())
source('paths.r')
source('functions/pic_pro.r')
source('functions/wrap_results_lm.r')
source('functions/wrap_results_pg.r')
library(data.table)
library(caper)

#set output path.----
pgls.output.path <- pgls_models_summary_aicc.path
  lm.output.path <-   lm_models_summary_aicc.path

#load trait data and phylogeny.----
d <- data.table(readRDS(inter_specific_analysis_data.path))
phy <- read.tree(phylogeny_raw.path)
aicc <- readRDS(aicc_model_comparison_plgs.path)

#Some data prep that should really be done somewhere else.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
rownames(d) <- d$Species
setnames(d,'root_lifespan','root.L')
d <- d[d$Species %in% phy$tip.label,] #drops 21 species.

#Fit pgls models based on aicc selected traits for a given dependent variable.----
traits <- c('Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots','log.LL','root.L')
pgls_fit <- list()
pgls_sum <- list()
for(i in 1:length(traits)){
  y <- traits[i]
  x <- aicc[[y]]$win_preds
  fit <- pic_pro(y=y, x=x, trait.data = d, phylogeny = phy)
  sum <- wrap_results_pg(fit, d)
  pgls_fit[[i]] <- fit
  pgls_sum[[i]] <- sum
  cat(traits[i],',',i,'of',length(traits),'traits fitted.\n')
}
names(pgls_fit) <- traits
pgls_sum <- do.call(plyr::rbind.fill,pgls_sum)
pgls_return <- list(pgls_fit,pgls_sum)
names(pgls_return) <- c('fit','summary')

#Fit lm models same way.----
lm_fit <- list()
lm_sum <- list()
for(i in 1:length(traits)){
  y <- traits[i]
  x <- aicc[[y]]$win_preds
  x <- paste(x, collapse = '+')
  form <- as.formula(paste0(y,'~',x))
  fit <- lm(form, data = d)
  sum <- wrap_results_lm(fit)
  lm_fit[[i]] <- fit
  lm_sum[[i]] <- sum
}
names(lm_fit) <- traits
lm_sum <- do.call(plyr::rbind.fill,lm_sum)
lm_return <- list(lm_fit,lm_sum)
names(lm_return) <- c('fit','summary')

#Save output.----
saveRDS(pgls_return, pgls.output.path)
saveRDS(  lm_return,   lm.output.path)

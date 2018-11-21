#this function takes a dataframe, a phylogeny, and some traits.
#trait.1 is the dependent varuiable.
#trait.2 - trait.N are predictors to be included in PGLS model.
#This function prunes the phylogeny, builds a comparative data object, and fits a PGLS model.

pic_pro <- function(y,x, phylogeny, trait.data, intercept = T, log = F){
  #make sure you aren't dealing with data.table.
  trait.data <- as.data.frame(trait.data)

  #subset data to only include species with complete data
  to_keep <- c(y,x)
  to_keep <- to_keep[!is.na(to_keep)]
  d.sub <- trait.data[,c(to_keep,'Species')]
  d.sub <- d.sub[complete.cases(d.sub),]
  
  #drop levels.
  d.sub <- droplevels.data.frame(d.sub)
  
  #prune the phylogeny to match.
  to.drop <- phylogeny$tip.label[!(phylogeny$tip.label %in% d.sub$Species)]
  n.phy <- drop.tip(phylogeny,to.drop)
  
  #make a comparative data object for PGLS in caper.
  c.data <- comparative.data(n.phy, d.sub, Species)
  
  #Cook up formula for PGLS
  linear <- paste0(paste0(to_keep[2:length(to_keep)], sep = '+'), collapse = '')
  linear <- substr(linear,1,nchar(linear)-1) #remove trailing '+'
  formula <- paste((y),'~')
  if(log == T){
    formula <- paste0('log(',y,') ~ ')
    if(y == 'log.LL'){
      formula <- paste((y),'~')
    }
  }
  formula <- paste(formula,linear)
  if(intercept == F){formula <- paste(formula,' - 1')}
  formula <- as.formula(formula)
  
  #run PGLS, return output
  model <- pgls(formula, data = c.data, lambda = 'ML')
  return(model)
  cat('cool job!')
}
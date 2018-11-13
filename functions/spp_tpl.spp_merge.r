#' spp_tpl.spp_merge.r
#' 4-criteria merge of two dataframes with two potential merge columns in each.
#' 1. Merge on 'Species' in x and y.
#' 2. For entries still unassigned, merge on 'Species' in x and 'tpl.Species' in y.
#' 3. For entries still unassigned, merge on 'tpl.Species' in x and 'Species' in y.
#' 4. For entries still unassigned, merge on 'tpl.Species' in x and y.
#'
#' @param d1 
#' @param d2 
#' @param merge.columns 
#'
#' @return
#' @export
#'
#' @examples
spp_tpl.spp_merge <- function(d1,d2,merge.columns=c('Species','tpl.Species')){
  d1 <- as.data.frame(d1)
  d2 <- as.data.frame(d2)
  ref_names <- colnames(d2)[!(colnames(d2) %in% merge.columns)]
  #merge 1.
  out <- merge(d1,d2[, !(colnames(d2) %in% merge.columns[2])], all.x=T)
  #merge 2.
  to_check <- colnames(out)[colnames(out) %in% ref_names[1]]
  assigned <- out[!is.na(out[,to_check]),]
  unassigned <- out[ is.na(out[,to_check]),]
  unassigned[,ref_names] = NULL
  unassigned <- merge(unassigned,d2[, !(colnames(d2) %in% merge.columns[1])], 
                      by.x = merge.columns[1], by.y = merge.columns[2], all.x = T)
  out <- rbind(assigned,unassigned)
  #merge 3.
  assigned <- out[!is.na(out[,to_check]),]
  unassigned <- out[ is.na(out[,to_check]),]
  unassigned[,ref_names] = NULL
  unassigned <- merge(unassigned,d2[, !(colnames(d2) %in% merge.columns[2])], 
                      by.x = merge.columns[2], by.y = merge.columns[1], all.x = T)
  out <- rbind(assigned,unassigned)
  #merge 4.
  assigned <- out[!is.na(out[,to_check]),]
  unassigned <- out[ is.na(out[,to_check]),]
  unassigned[,ref_names] = NULL
  unassigned <- merge(unassigned,d2[, !(colnames(d2) %in% merge.columns[1])], 
                      by.x = merge.columns[2], by.y = merge.columns[2], all.x = T)
  out <- rbind(assigned,unassigned)
  #return output.
  return(out)
}
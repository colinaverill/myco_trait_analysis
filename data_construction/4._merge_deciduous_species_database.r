#Merging plant deciduous databases.
rm(list=ls())
source('paths.r')

#set output path.----
output.path <- merged_decid_traits.path

#load cleaned data.----
zan <- readRDS(zanne_decid_clean.path)
try <- readRDS(  try_decid_clean.path)

#merge databases.----
out <- zan
out <- rbind(out,try[!(try$Species %in% zan$Species),])

#Save output.----
saveRDS(out,output.path)

#Report.----
n.spp <- nrow(out)
cat(n.spp,'unique species have deciduous assignments. This is approximately',round(n.spp/400000,2)*100,'% of plant species on Earth.')

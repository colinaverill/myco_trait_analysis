#processing trait data from Sun 2018 PNAS. 10.1073/pnas.1716595115
#Species level means of sencent and root N and P. 
rm(list=ls())
library(data.table)
source('paths.r')

#set output path.----
output.path <- sun_PNAS2018_clean.path

#load data.----
d <- data.table(read.csv(sun_PNAS2018_raw.path))
d$doi <- '10.1073/pnas.1716595115'
setnames(d,'Species.Name','Species')

#Save output.----
saveRDS(d,output.path)
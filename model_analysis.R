library(sp)
library(rgdal)
library(gstat)
library(rgeos)

inroot <- "D:/Studie/urban_sprawl_group6/results_final/" # change/omit as needed
deter15 <- readGDAL(paste0(inroot, "perc_builtup_15_deterministic.asc_corrected")) # deterministic run

# Read all model results into a matrix "fr"
flist <- list.files(inroot) # all file names in directory
flist <- flist[grep("_corrected", flist)] # only retain corrected
fr = numeric()

for(in_name in flist)
{
  in_name <- paste0(inroot, in_name)
  fr <- cbind(fr,(readGDAL(in_name, silent = TRUE))$band1)
}
fr[is.na(fr)] <- 0 # set NA to 0
# Compute probability of urbanization (threshold @25% occ.)
prob <- apply(fr, 1, function(x) mean(ifelse(x > 25, 1, 0)))
# Convert prob to SpatialPixelsDataFrame
prob <- SpatialPixelsDataFrame(coordinates(deter15),
                               as.data.frame(prob))
# plot using blue-pink-yellow colorramp
spplot(prob, col.regions=bpy.colors())
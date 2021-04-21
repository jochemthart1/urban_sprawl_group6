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


unc_hom = numeric()
unc_norm = numeric()
fix_norm = numeric()

for(in_name in flist)
{
  in_name <- paste0(inroot, in_name)
  if (grepl("fixed_normal-distribution", in_name, fixed=TRUE)) {
    fix_norm <- cbind(fix_norm,(readGDAL(in_name, silent = TRUE))$band1)
  }
  else if (grepl("uncertain_homogeneous", in_name, fixed=TRUE)) {
    unc_hom <- cbind(unc_hom,(readGDAL(in_name, silent = TRUE))$band1)
  }
  else if (grepl("uncertain_normal-distribution", in_name, fixed=TRUE)) {
    unc_norm <- cbind(unc_norm,(readGDAL(in_name, silent = TRUE))$band1)
  }
}

unc_hom[is.na(unc_hom)] <- 0 # set NA to 0
unc_norm[is.na(unc_norm)] <- 0 # set NA to 0
fix_norm[is.na(fix_norm)] <- 0 # set NA to 0

mean_func <- function(data) {
  mean_grid <- apply(data, 1, function(x) mean(x))
  mean_grid <- SpatialPixelsDataFrame(coordinates(deter15),
                                      as.data.frame(mean_grid))
  spplot(mean_grid, col.regions=bpy.colors())
  return(mean_grid)
}

var_func <- function(data) {
  var_grid <- apply(data, 1, function(x) var(x))
  var_grid <- SpatialPixelsDataFrame(coordinates(deter15),
                                       as.data.frame(var_grid))
  spplot(var_grid, col.regions=bpy.colors())
  return(var_grid)
}

mean_unc_hom <- mean_func(unc_hom)
spplot(mean_unc_hom, col.regions=bpy.colors(), main = "plot mean uncertain homogeneous")

mean_unc_norm <- mean_func(unc_norm)
spplot(mean_unc_norm, col.regions=bpy.colors(), main = "plot mean uncertain normal-dist")

mean_fix_norm <- mean_func(fix_norm)
spplot(mean_fix_norm, col.regions=bpy.colors(), main = "plot mean fixed normal-dist")

var_unc_hom <- var_func(unc_hom)
spplot(var_unc_hom, col.regions=bpy.colors(), main = "plot var uncertain homogeneous")

var_unc_norm <- var_func(unc_norm)
spplot(var_unc_norm, col.regions=bpy.colors(), main = "plot var uncertain normal-dist")

var_fix_norm <- var_func(fix_norm)
spplot(var_fix_norm, col.regions=bpy.colors(), main = "plot var fixed normal-dist")

terrain_contrib <- 100 * ((1 - var_unc_hom) / var_unc_norm)
pop_contrib <- 100 * ((1 - var_fix_norm) / var_unc_norm)

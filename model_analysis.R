library(sp)
library(rgdal)
library(gstat)
library(rgeos)
library(raster)

inroot <- "D:/Studie/urban_sprawl_group6/results_final/" # change/omit as needed
deter15 <- readGDAL(paste0(inroot, "perc_builtup_15_deterministic.asc_corrected")) # deterministic run

spplot(deter15, col.regions=bpy.colors(), main = "plot deterministic model")

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

temp <- unc_norm

unc_hom[is.na(unc_hom)] <- 0 # set NA to 0
unc_norm[is.na(unc_norm)] <- 0 # set NA to 0
fix_norm[is.na(fix_norm)] <- 0 # set NA to 0

mean_func <- function(data) {
  mean_grid <- apply(data, 1, function(x) mean(x))
  mean_grid <- SpatialPixelsDataFrame(coordinates(deter15),
                                      as.data.frame(mean_grid))
  return(mean_grid)
}

var_func <- function(data) {
  var_grid <- apply(data, 1, function(x) var(x))
  var_grid <- SpatialPixelsDataFrame(coordinates(deter15),
                                       as.data.frame(var_grid))
  return(var_grid)
}

sd_func <- function(data) {
  sd_grid <- apply(data, 1, function(x) sd(x))
  sd_grid <- SpatialPixelsDataFrame(coordinates(deter15),
                                     as.data.frame(sd_grid))
  return(sd_grid)
}

mean_unc_hom <- mean_func(unc_hom)
spplot(mean_unc_hom, col.regions=bpy.colors(), main = "plot mean uncertain homogeneous")

mean_unc_norm <- mean_func(unc_norm)
spplot(mean_unc_norm, col.regions=bpy.colors(), main = "plot mean uncertain normal-dist")

mean_fix_norm <- mean_func(fix_norm)
spplot(mean_fix_norm, col.regions=bpy.colors(), main = "plot mean fixed normal-dist")

#question 6
rural95 <- mean_func(temp)

rural95 <- apply(rural95@data, 1, function(x) ifelse(x < 5, 1, 0))
rural95 <- SpatialPixelsDataFrame(coordinates(deter15),
                               as.data.frame(rural95))
spplot(rural95, col.regions=bpy.colors(), main = "plot both uncertain with 95% probability rural")

deter95 <- apply(deter15@data, 1, function(x) ifelse(x < 5, 1, 0))
deter95 <- SpatialPixelsDataFrame(coordinates(deter15),
                                  as.data.frame(deter95))
spplot(deter95, col.regions=bpy.colors(), main = "plot deterministic with 95% probability rural")

var_unc_hom <- var_func(unc_hom)
spplot(var_unc_hom, col.regions=bpy.colors(), main = "plot var uncertain homogeneous")

var_unc_norm <- var_func(unc_norm)
spplot(var_unc_norm, col.regions=bpy.colors(), main = "plot var uncertain normal-dist")

var_fix_norm <- var_func(fix_norm)
spplot(var_fix_norm, col.regions=bpy.colors(), main = "plot var fixed normal-dist")

sd_unc_hom <- sd_func(unc_hom)
spplot(sd_unc_hom, col.regions=bpy.colors(), main = "plot sd uncertain homogeneous")

sd_unc_norm <- sd_func(unc_norm)
spplot(sd_unc_norm, col.regions=bpy.colors(), main = "plot sd uncertain normal-dist")

sd_fix_norm <- sd_func(fix_norm)
spplot(sd_fix_norm, col.regions=bpy.colors(), main = "plot sd fixed normal-dist")

pop_contrib <- 100*(1-var_unc_hom@data/var_unc_norm@data)
coordinates(pop_contrib) <- coordinates(deter15)
gridded(pop_contrib) <- T
pop_contrib$var_grid[pop_contrib$var_grid < -100] <- -100
spplot(pop_contrib, col.regions=bpy.colors(), main = "plot population contribution %")

terrain_contrib <- 100*(1-var_fix_norm@data/var_unc_norm@data)
coordinates(terrain_contrib) <- coordinates(deter15)
gridded(terrain_contrib) <- T
terrain_contrib$var_grid[terrain_contrib$var_grid < -100] <- -100
spplot(terrain_contrib, col.regions=bpy.colors(), main = "plot terrain contribution %")

contrib <- terrain_contrib@data + pop_contrib@data
coordinates(contrib) <- coordinates(deter15)
gridded(contrib) <- T
spplot(contrib, col.regions=bpy.colors(), main = "plot total contribution %")
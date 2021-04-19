# read ahn elevation data
ahn <- readGDAL("ahn100_f.asc")
# Add residuals to AHN and make SpatialPixelsDataFrame object
# it is assumed that predicted residuals are in "resid"
defaultdem <- data.frame(band1 = ahn$band1 + resid$var1.pred)
coordinates(defaultdem) <- coordinates(ahn)
gridded(defaultdem) <- TRUE
spplot(defaultdem, col.regions=bpy.colors())
write.asciigrid(defaultdem, "defaultdem.asc") # write to disk

outroot = "D:/mydir/mysubdir/dem" # (mind forward slashes)
for(i in 1:100)
{
  outname <- paste0(outroot, i, ".asc")
  outdem <- data.frame(band1 = ahn$band1 + residsim[[i]])
  coordinates(outdem) <- coordinates(ahn)
  gridded(outdem) <- T
  write.asciigrid(outdem, outname)
}
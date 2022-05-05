#' GridFilter: Intersect Shape with a Grid and Exclude Cells Based on Area Coverage
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2014/12/gridfilter-intersect-grid-with-shape.html
#' @description Creates a shape intersected with a grid. The user can exclude cells based on area coverage. For example, if the shape covers only 50 percent of some cells, the user can choose to exclude or maintain these cells.
#' @return A "gridded" shapefile. Plot this output to take a look at it.
#' @param shape shapefile
#' @param resol resolution
#' @param prop minimum value of area covered by the grid cell. The default is 0 (i.e. it does not delete any grid cell)
GridFilter<-function(shape, resol = 1, prop = 0){
  grid <- raster::raster(raster::extent(shape))
  raster::res(grid) <- resol
  sp::proj4string(grid)<-sp::proj4string(shape)
  gridpolygon <- raster::rasterToPolygons(grid)
  drylandproj<-sp::spTransform(shape, sp::CRS("+proj=laea"))
  gridpolproj<-sp::spTransform(gridpolygon, sp::CRS("+proj=laea"))
  gridpolproj$layer <- c(1:length(gridpolproj$layer))
  areagrid <- rgeos::gArea(gridpolproj, byid=T)
  dry.grid <- raster::intersect(drylandproj, gridpolproj)
  areadrygrid <- rgeos::gArea(dry.grid, byid=T)
  info <- cbind(dry.grid$layer, areagrid[dry.grid$layer], areadrygrid)
  dry.grid$layer<-info[,3]/info[,2]
  dry.grid <- sp::spTransform(dry.grid, sp::CRS(sp::proj4string(shape)))
  dry.grid.filtered <- dry.grid[dry.grid$layer >= prop,]}

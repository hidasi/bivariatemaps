#' GridFilter: Intersect a Shape with a Grid and Exclude Cells Based on Area Coverage
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2014/12/gridfilter-intersect-grid-with-shape.html
#' @description Overlay a shapefile with a grid. Then, excludes grid cells that do not cover a specific amount of the Shape area.
#' @return A gridded shapefile.
#' @param shape A shapefile.
#' @param resol The resolution (in degrees) of the grid you want to overlay. The default is 1 degree of longitude and latitude.
#' @param prop The minimum value of area covered by the grid cell. The default is 0 (i.e. it does not delete any grid cell).
GridFilter <- function(shape, resol = 1, prop = 0) {
  # Create a raster grid based on the extent of the shape with the specified resolution
  grid <- terra::rast(terra::ext(shape), res = resol)
  # Set the projection of the grid to match the shape
  terra::crs(grid) <- terra::crs(shape)
  # Convert the raster grid to polygons
  gridpolygon <- terra::as.polygons(grid)
  # Reproject both the shape and the grid polygons to the LAEA projection
  drylandproj <- terra::project(shape, "+proj=laea")
  gridpolproj <- terra::project(gridpolygon, "+proj=laea")
  # Assign a layer ID to each grid polygon
  gridpolproj$layer <- base::seq_len(nrow(gridpolproj))
  # Calculate the area of each grid polygon in the new projection
  areagrid <- terra::expanse(gridpolproj, unit = "m")
  # Intersect the shape with the grid polygons
  dry.grid <- terra::intersect(drylandproj, gridpolproj)
  # Calculate the area of each intersected polygon (dry grid)
  areadrygrid <- terra::expanse(dry.grid, unit = "m")
  # Combine information: layer ID, total grid area, and dry grid area
  info <- cbind(dry.grid$layer, areagrid[dry.grid$layer], areadrygrid)
  # Calculate the proportion of the dry grid area relative to the total grid area
  dry.grid$layer <- info[, 3] / info[, 2]
  # Reproject the result back to the original projection of the input shape
  dry.grid <- terra::project(dry.grid, terra::crs(shape))
  # Filter the grid polygons based on the proportion threshold
  dry.grid.filtered <- dry.grid[dry.grid$layer >= prop, ]
  return(dry.grid.filtered)
}

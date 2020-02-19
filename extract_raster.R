# alternative approach using rasterize()

extract_aggr <- function(polygon, raster) { 
    ext = extent(polygon) 
    raster_crop = crop(raster, ext)
    polygon_raster = rasterize(polygon, raster_crop, field = 1) # Rasterize Polygons (check: fasterize::fasterize)
    polygon_values = getValues(raster_crop)[which(polygon_raster@data@values == 1)]
    return(mean(polygon_values,na.rm=TRUE))
}

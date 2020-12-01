###########
#
# Function that calculates species richness 
#
###########
#
sp.rich <- function(X, cell)
	{
		library(raster)
		rast <- raster()
		extent(rast) <- c(-180,+180,-90,+90)
		res(rast) <- cell
		x <- split(X, X$sp)
		x <- lapply(lapply(x,'[', c(3,2)), SpatialPoints, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
		z <- list()
			for (i in 1:length(x))
				{
	 				z[[i]] = unique(cellFromXY(rast, xy = x[[i]])) 
				}
		t <- rle(sort(unlist(z)))
			for (i in 1:max(t$lengths)) 
				{
					rast[t$values[t$lengths==i]] <- i
				}
		return(rasterToPolygons(rast))
	}
#
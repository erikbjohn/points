#' \code{outlines} package
#'
#' outlines
#'
#' See the Vignette on
#'
#' @docType package
#' @name outlines
#' @importFrom dplyr %>% select
#' @importFrom data.table :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @title address
#'
#' @description Creates and loads outlines.address file
#' @param data.path character vector
#' @param fresh logical TRUE for fresh run
#' @keywords outliens, clean
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     foreign
#'     parcels
#' @importFrom dplyr select one_of
address <- function(data.path, fresh=FALSE){
  raw.path <- paste0(data.path, '/raw/')
  clean.path <- paste0(data.path, '/clean/address/')
  raw <- list(boulder = paste0(raw.path, ''),
              denver = paste0(raw.path, ''))
  clean <- list(address = paste0(clean.path, 'outlines.address.rdata'))
  l.path <- list(raw=raw,clean=clean)            
  
  # ROxygen initialize
    if(file.exists(l.path$clean$address)){
      load(l.path$clean$address)
    } else {
      outlines.address <- parcels::address(str_replace(data.path, 'outlines', 'parcels'))
      outlines.address <- dplyr::select(outlines.address,
                                        one_of(names(outlines.address)[!names(outlines.address)%in%c('location.street.direction')]))
      
      save(outlines.address,file=l.path$clean$address)
    }
    return(outlines.address)
}
#' @title address.update
#'
#' @description updates outlines.address with point data
#' @param data.path source path for the package
#' @param outlines.address the outlines.address data.table
#' @param outlines.address.new the new (geocoded outlines.address)
#' @param outline.source the source type for the outline (default to points here)
#' @keywords outlines, points, update
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     foreign
#' @importFrom dplyr select one_of
address.update <- function(data.path, outlines.address, outlines.address.new, outline.source='points'){
  street.num <- NULL
  raw.path <- paste0(data.path, '/raw/')
  clean.path <- paste0(data.path, '/clean/address/')
  raw <- list(boulder = paste0(raw.path, ''),
              denver = paste0(raw.path, ''))
  clean <- list(address = paste0(clean.path, 'outlines.address.rdata'))
  l.path <- list(raw=raw,clean=clean) 
  if(missing(outlines.address)){
    if(file.exists(l.path$clean$address)){
      load(l.path$clean$address)
    } else {
      outlines.address <- outlines.address.new
    }
  }
  outlines.address.new$location.source <- outline.source
  outlines.address.new$location.type <- 'outlines'
  # Clean cols
  cols <- names(outlines.address)[!(names(outlines.address) %in% c('point.id',
                                                                   'location.street.num',
                                                                   'location.street.direction'))]
  outlines.address <- outlines.address[, (cols), with=FALSE]
  outlines.address.new <- outlines.address.new[, (cols), with=FALSE]
  outlines.address <- rbindlist(list(outlines.address, outlines.address.new), use.names=TRUE)
  # Remove duplicates
  col.keys <- names(outlines.address)[!names(outlines.address) %in% c('location.id')]
  setkeyv(outlines.address, col.keys)
  outlines.address <- unique(outlines.address)
  outlines.address <- outlines.address[, street.num:=as.numeric(street.num)]
  save(outlines.address, file=l.path$clean$address)
  return(outlines.address)
}
#' @title shapes
#'
#' @description currently only returns denver building footprint shapes
#' @param data.path character vector
#' @param fresh logical TRUE for fresh run
#' @keywords outlines, clean, shapes
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     foreign
#'     parallel
#'     sp
#'     methods
#' @importFrom dplyr select one_of
#' @importFrom grDevices chull
shapes <- function(data.path, fresh=FALSE){
  raw.path <- paste0(data.path, '/raw/')
  clean.path <- paste0(data.path, '/clean/shapes/')
  raw <- list(boulder = paste0(raw.path, 'boulder/boulder.outlines'),
              denver = paste0(raw.path, 'denver/building_outlines_2014'))
  clean <- list(outlines = paste0(clean.path, 'shapes.outlines.rdata'),
                boulder = paste0(clean.path, 'shapes.outlines.boulder.rds'),
                denver = paste0(clean.path, 'shapes.outlines.denver.rds'),
                osm = paste0(clean.path, 'shapes.outlines.osm.rds'))
  l.path <- list(raw=raw,clean=clean)  
  bbox <- NULL; GridTopology <- NULL; SpatialPoints <- NULL
  CRS <- NULL; proj.env <- NULL; coordinates <- NULL
  chull <- NULL;  SpatialPolygons <- NULL
  Polygons <- NULL; Polygon <- NULL
  proj4string <- NULL; slot <- NULL
  detectCores <- NULL; over <- NULL
  if (file.exists(l.path$clean$outlines) & fresh==FALSE) {
    load(l.path$clean$outlines)
  } else {
    l.outlines <- list()
    l.outlines$boulder <- shapes.boulder(l.path)
    # Stick to denver outlines for now
    #l.outlines$denver <- shapes.denver(l.path)
    #l.outlines$osm <- shapes.osm(l.path)
    
    # Denver/OSM 
    res <- 1000            ## Distance between grid points (30 in OP's question) 
    BB <- bbox(l.outlines$denver)
    BB <- res*round(BB/res) ## Pretty up the bounding box
    GT <- GridTopology(cellcentre.offset = BB[,1], 
                       cellsize = c(res, res),
                       cells.dim = (c(diff(BB[1,]), diff(BB[2,]))/res) + 1)
    SP <- SpatialPoints(l.outlines$denver, proj4string = CRS(proj.env))
    x <- coordinates(SP)[,1]
    y <- coordinates(SP)[,2]
    z <- chull(x,y)
    dfHull <- cbind(x[z], y[z])
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(dfHull)), ID=1)))
    proj4string(sp_poly) <- CRS(proj.env)
    osm.clip <- gIntersection(l.outlines$osm, sp_poly, byid=TRUE, checkValidity = TRUE)
    osm.ids <- sapply(l.outlines$osm@polygons, function(x) slot(x, 'ID'))
    osm.clip.ids <- str_replace(sapply(osm.clip@polygons, function(x) slot(x, 'ID')), ' 1', '')
    osm.convex.ids <- which(osm.ids %in% osm.clip.ids)
    shape.osm.in.denver <- l.outlines$osm[osm.convex.ids,]
    start <- seq(1, length(shape.osm.in.denver), 1000)
    end <- c(start[2:length(start)], length(shape.osm.in.denver))
    seqs <- data.table(start=start, end=end)
    l.shapes.osm.in.denver <- list()
    for (i in 1:nrow(seqs)){
      seq <- seqs[i]
      l.shapes.osm.in.denver[[i]] <- shape.osm.in.denver[seq$start:seq$end,]
    }
    no_cores <- detectCores() - 1
    l.ids <- mclapply(l.shapes.osm.in.denver, function(x) sp::over(l.outlines$denver, x), mc.cores = no_cores)
    l.ids.bk <- l.ids
    l <- lapply(l.ids, function(x) x$id)
    ids <- unlist(l)
    osm.redundant.ids <- ids[!is.na(ids)]
    osm.keep.ids <- osm.ids[!(osm.ids %in% osm.redundant.ids)]
    crop.outlines.osm.denver <- l.outlines$osm[osm.keep.ids,]
    shapes.outlines.denver.osm <- gUnion(crop.outlines.osm.denver, l.outlines$denver)
    
    # Boulder/OSM
    shp.boulder.outline <- l.outlines$boulder
    res <- 1000            ## Distance between grid points (30 in OP's question) 
    BB <- bbox(shp.boulder.outline)
    BB <- res*round(BB/res) ## Pretty up the bounding box
    GT <- GridTopology(cellcentre.offset = BB[,1], 
                       cellsize = c(res, res),
                       cells.dim = (c(diff(BB[1,]), diff(BB[2,]))/res) + 1)
    SP <- SpatialPoints(shp.boulder.outline, proj4string = CRS(proj.env))
    x <- coordinates(SP)[,1]
    y <- coordinates(SP)[,2]
    z <- chull(x,y)
    dfHull <- cbind(x[z], y[z])
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(dfHull)), ID=1)))
    proj4string(sp_poly) <- CRS(proj.env)
    osm.clip <- gIntersection(l.outlines$osm, sp_poly, byid=TRUE, checkValidity = TRUE)
    # osm.clip.ids <- str_replace(sapply(osm.clip@polygons, function(x) slot(x, 'ID')), ' 1', '')
    # osm.ids <- sapply(l.outlines$osm@polygons, function(x) slot(x, 'ID'))
    # osm.convex.ids <- which(osm.ids %in% osm.clip.ids)
    # shape.osm.in.boulder <- l.outlines$osm[osm.convex.ids,]
    # start <- seq(1, length(shape.osm.in.boulder), 1000)
    # end <- c(start[2:length(start)], length(shape.osm.in.boulder))
    # seqs <- data.table(start=start, end=end)
    # l.shapes.osm.in.boulder <- list()
    # for (i in 1:nrow(seqs)){
    #     seq <- seqs[i]
    #     l.shapes.osm.in.boulder[[i]] <- shape.osm.in.boulder[seq$start:seq$end,]
    # }
    # no_cores <- detectCores() - 1
    # l.ids <- mclapply(l.shapes.osm.in.boulder, function(x) over(shp.boulder.outline, x), mc.cores = no_cores)
    # l.ids.bk <- l.ids
    # l <- lapply(l.ids, function(x) x$id)
    # ids <- unlist(l)
    # osm.redundant.ids <- ids[!is.na(ids)]
    # osm.keep.ids <- osm.ids[!(osm.ids %in% osm.redundant.ids)]
    # crop.outlines.osm.boulder <- l.outlines$osm[osm.keep.ids,]
    shapes.outlines.boulder.osm <- gUnion(osm.clip, shp.boulder.outline)
    shapes.outlines.boulder.osm <- gUnaryUnion(shapes.outlines.boulder.osm)
    shapes.outlines.boulder.osm <- disaggregate(shapes.outlines.boulder.osm)
    df <- data.frame(id = 1:length(shapes.outlines.boulder.osm))
    shapes.outlines.boulder.osm <- SpatialPolygonsDataFrame(shapes.outlines.boulder.osm, df)
  #  writeOGR(shapes.outlines.boulder.osm, ".", "../CleanData/EsriShapes/shapes.outlines.boulder.osm", driver="ESRI Shapefile")
    
    # Combine all
    shapes.outlines <- gUnion(shapes.outlines.boulder.osm, shapes.outlines.denver.osm)
  }
  return(shapes.outlines)
}
#' @title shapes.boulder
#'
#' @description Not currently running! 
#' @param l.path character vector
#' @param fresh logical TRUE for fresh run
#' @keywords outlines, clean, shapes
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     methods.shapes
#'     foreign
#'     cleangeo
#'     methods
#'     rgdal
#' @importFrom dplyr select one_of
shapes.boulder <- function(l.path, fresh=FALSE){
  bbox <- NULL; GridTopology <- NULL; SpatialPoints <- NULL
  CRS <- NULL; proj.env <- NULL; coordinates <- NULL
  chull <- NULL;  SpatialPolygons <- NULL
  Polygons <- NULL; Polygon <- NULL
  proj4string <- NULL; slot <- NULL
  detectCores <- NULL; over <- NULL
  if (file.exists(l.path$clean$shapes) & fresh==FALSE){
    shapes.outlines.boulder <- readRDS(l.path$clean$shapes)
  } else {
    ## Boulder
    shape.dir <- l.path$raw$boulder
    shapes.outlines.boulder <- readOGR(dsn = shape.dir, layer = 'City_of_Boulder_3D_Building_Roof', verbose = FALSE)
    shapes.outlines.boulder <- clgeo_Clean(shapes.outlines.boulder)
    
    #********** RIGHT AROUND HERE ************
    shapes.outlines.boulder <- methods.shapes::clean.shape(shapes.outlines.boulder, proj.env)
    
    shapes.outlines.boulder <- gUnaryUnion(shapes.outlines.boulder)
    save(shapes.outlines.boulder, '../RawData/shapes.outlines.boulder.Union.rdata')
    shapes.outlines.boulder <- disaggregate(shapes.outlines.boulder)
    #saveRDS(shapes.outlines.boulder, file=l.pat)
    
    polys <- shapes.outlines.boulder@polygons[[1]]@Polygons
    pl <- vector("list", length(polys))
    for (i in 1:length(polys)) { pl[i] <- Polygons(list(polys[[i]]), i) }
    b.spolys <- SpatialPolygons(pl)
    row.ids <- sapply(slot(b.spolys, "polygons"), function(i) slot(i, "ID"))    
    b.exploded <- SpatialPolygonsDataFrame(b.spolys, data.frame(FID=as.numeric(row.ids))) 

    # Clip and clean
    #shapes.zoning <- funShapes.zoning()
    #df.boulder.zoning <- over(shapes.outlines.boulder, shapes.zoning)
    #zoned.outlines <- which(!is.na(df.boulder.zoning$REG_CLASS))
    #shapes.outlines.boulder <- shapes.outlines.boulder[zoned.outlines,]
    # Did the dissolve in QGIS (reimport here)
    writeOGR(shapes.outlines.boulder, ".", "../CleanData/EsriShapes/shapes.outlines.boulder.sub", driver="ESRI Shapefile")
    shape.dir <- '../CleanData/EsriShapes/'
    shapes.outlines.boulder <- readOGR(dsn = shape.dir, layer = 'shapes.outlines.boulder.dissolve', verbose = FALSE)
    shapes.outlines.boulder <- methods.shapes::clean.shape(shapes.outlines.boulder, proj.env)
    shapes.outlines.boulder <- cleangeo::clgeo_Clean(shapes.outlines.boulder)
    shapes.outlines.boulder <- disaggregate(shapes.outlines.boulder)
    DT <- data.frame(ID = paste0('boulder.',seq(1:length(shapes.outlines.boulder))))
    # And add the new ID
    shapes.outlines.boulder <- SpatialPolygonsDataFrame(shapes.outlines.boulder, DT)
    saveRDS(shapes.outlines.boulder, file = l.path$clean$boulder)
  #  writeOGR(shapes.outlines.boulder, ".", "../CleanData
  #           /EsriShapes/shapes.outlines.boulder", driver="ESRI Shapefile")
  }
  return(shapes.outlines.boulder)
}
#' @title shapes.denver
#'
#' @description loads denver oulines as spatialPolygonDataFrame
#' @param l.path character vector package data path
#' @param fresh logical TRUE for fresh run
#' @keywords outlines, clean, shapes
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     methods.shapes
#'     foreign
#'     methods.shapes
#'     rgeos
#'     methods
#'     rgdal
#' @importFrom dplyr select one_of group_by summarise first
shapes.denver <- function(l.path, fresh=FALSE){
  bbox <- NULL; GridTopology <- NULL; SpatialPoints <- NULL
  CRS <- NULL; proj.env <- NULL; coordinates <- NULL
  chull <- NULL;  SpatialPolygons <- NULL
  Polygons <- NULL; Polygon <- NULL
  proj4string <- NULL; slot <- NULL
  detectCores <- NULL; over <- NULL
  spTransform <- NULL; bldg.type <- NULL
  outline.id <- NULL;
  if (file.exists(l.path$clean$denver) & fresh==FALSE){
    shapes.outlines.denver <- readRDS(l.path$clean$denver)
  } else {
    print('building shapes.outlines')
    shape.dir <- l.path$raw$denver
    shapes.outlines <- readOGR(dsn = shape.dir,
                               layer = 'building_outlines_2014', verbose = FALSE)
    shapes.outlines.proj.orig <- proj4string(shapes.outlines) # Original projections
    shapes.outlines.proj.new <- methods.shapes::shapes.proj.env
    
    shapes.outlines <- shapes.outlines %>%
      spTransform(CRS(shapes.outlines.proj.new))
    
    names(shapes.outlines@data)<- gsub('\\_', '.', str_to_lower(names(shapes.outlines@data)))
    names(shapes.outlines@data)[names(shapes.outlines@data)=='building.i'] <- 'outline.id'
    bldg.types <- c('Commercial', 'Industrial', 'Medical', 'Misc', 'Public', 'Residential')
    shapes.outlines <- shapes.outlines[shapes.outlines@data$bldg.type %in% bldg.types,]
    # hold data to bring back in
    data.outlines <- as.data.table(shapes.outlines@data)
    data.outlines <- data.outlines %>%
      group_by(outline.id=as.character(outline.id)) %>%
      dplyr::summarise(outline.type = first(bldg.type))
    data.outlines <- as.data.table(data.outlines)
    setkey(data.outlines, outline.id)
    
    # # Dissolve to unique buildings
    shapes.outlines <- gUnaryUnion(shapes.outlines,id = shapes.outlines@data$outline.id)
    shapes.outlines.ids <- sapply(slot(shapes.outlines, "polygons"), function(x) slot(x, "ID"))
    new.data <- data.table(outline.id = shapes.outlines.ids, outline.area = round(gArea(shapes.outlines, byid=TRUE),2))
    setkey(new.data, outline.id)
    new.data <- data.outlines[new.data]
    row.names(new.data) <- row.names(shapes.outlines)
    shapes.outlines.denver <- SpatialPolygonsDataFrame(shapes.outlines, data=new.data)
    shapes.outlines.denver@data$outline.area <- round(gArea(shapes.outlines, byid=TRUE),2)
    shapes.outlines.denver@data$outline.source <- 'Denver'
    saveRDS(shapes.outlines.denver, file=l.path$clean$denver)
   # writeOGR(shapes.outlines.denver, ".", "../CleanData/EsriShapes/shapes.outlines.denver", driver="ESRI Shapefile")
  }
  return(shapes.outlines.denver)
}
#' @title shapes.osm
#'
#' @description Not currently running
#' @param l.path character vector
#' @param fresh logical TRUE for fresh run
#' @keywords outlines, clean, shapes
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     methods.shapes
#'     foreign
#'     cleangeo
#'     methods.shapes
#'     rgeos
#' @importFrom dplyr select one_of group_by summarise first
shapes.osm <- function(l.path, fresh=FALSE){
  # bbox <- NULL; GridTopology <- NULL; SpatialPoints <- NULL
  # CRS <- NULL; proj.env <- NULL; coordinates <- NULL
  # chull <- NULL;  SpatialPolygons <- NULL
  # Polygons <- NULL; Polygon <- NULL
  # proj4string <- NULL; slot <- NULL
  # detectCores <- NULL; over <- NULL
  # if (file.exists(l.path$clean$osm) & fresh ==FALSE){
  #   shapes.outlines.osm <- readRDS(l.path$clean$osm)
  # } else {
  #   # Create Bounding Box params
  #   l.bbox <- list()
  #   l.bbox$long$start <- -105.30
  #   l.bbox$long$end <- - 104.75
  #   l.bbox$long$step <- 0.01214
  #   l.bbox$lat$start <- 39.55
  #   l.bbox$lat$end <- 40.1
  #   l.bbox$lat$step <- 0.01031
  #   l.coords <- list() 
  #   l.coords$long <- seq(l.bbox$long$start, l.bbox$long$end, l.bbox$long$step)
  #   l.coords$lat <- seq(l.bbox$lat$start, l.bbox$lat$end, l.bbox$lat$step)
  #   nLongs <- length(l.coords$long)
  #   nLats <- length(l.coords$lat)
  #   l.box <- list()
  #   i <- 0
  #   for (iLong in 1:(nLongs-1)){
  #     for(iLat in 1:(nLats-1)){
  #       i <- i + 1
  #       l <- list()
  #       l$left <- l.coords$long[iLong]
  #       l$bottom <- l.coords$lat[iLat]
  #       l$right <- l.coords$long[iLong+1]
  #       l$top <- l.coords$lat[iLat+1]
  #       l$iter <- i
  #       l.box[[i]] <- l
  #     } 
  #   }
  #   no_cores <- detectCores() - 1
  #   outlines <- mclapply(l.box, funScrape.osm, proj.env = proj.env, mc.cores = no_cores)
  #   outlines <- outlines[!sapply(outlines, is.null)]
  #   outlines <- funShapes.union.all.iter(outlines)
  #   shapes.outlines.osm <- outlines[[1]]
  #   saveRDS(shapes.outlines.osm, file=l.path$clean$osm)
  #  # writeOGR(shapes.outlines.osm, ".", "../CleanData/EsriShapes/shapes.outlines.osm", driver="ESRI Shapefile")
  # }
  # return(shapes.outlines.osm)
}
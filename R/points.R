#' \code{points} package
#'
#' points
#'
#' See the Vignette on 
#'
#' @docType package
#' @name points
#' @importFrom dplyr %>% select
#' @importFrom data.table :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @title address
#'
#' @description Creates and loads points.address file
#' @param data.path character vector
#' @param fresh logical TRUE for fresh run
#' @keywords points, clean, geocode
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
  clean <- list(address = paste0(clean.path, 'points.address.rdata'))
  l.path <- list(clean=clean)       
  if (file.exists(l.path$clean$address)){
    #print('loading points.address')
    load(l.path$clean$address)
  } else {
    # Initalize based off the structure of parcels
    parcels.address <- parcels::address(str_replace(data.path, 'points', 'parcels'))
    points.address <- data.table::copy(parcels.address[FALSE])
    points.address <- data.table(points.address, long=as.character(), lat=as.character(), match.descr=character(), match.rank=character())
    save(points.address, file=l.path$clean$address)
  }
  return(points.address)
}
#' @title address.update
#'
#' @description Creates/loads and updates points.address file
#' @param data.path character vector
#' @param points.address.new new points 
#' @param points.source geocoded points
#' @param points.address the original points.address files
#' @keywords points, clean, address, update
#' @export
#' @import stringr
#'     data.table
#'     stringdist
#'     methods.string
#'     foreign
#' @importFrom dplyr select one_of
#' @importFrom parcels address
address.update <- function(data.path, points.address.new, points.source='geocode.points',points.address){
  lat <- NULL; long <- NULL; location.id <- NULL; street.num <- NULL
  raw.path <- paste0(data.path, '/raw/')
  clean.path <- paste0(data.path, '/clean/address/')
  clean <- list(address = paste0(clean.path, 'points.address.rdata'))
  l.path <- list(clean=clean)      
  if(missing(points.address)){
      points.address <- points::address(data.path)
  }
  cols <- names(points.address)[!(names(points.address)%in%c('location.street.num','location.street.direction'))]
  if (nrow(points.address)>0){
    location.id.start <- max(as.integer(points.address$location.id))
    # Clean columns for integrity
    points.address <- points.address[,(cols),with=FALSE]
    cols <- names(points.address)[!(names(points.address)%in%c('location.street.num','location.street.direction'))]
    points.address <- points.address[,(cols),with=FALSE]
    ids.min <- max(as.integer(points.address$location.id))+1
    ids.max <- ids.min + nrow(points.address.new)-1
    location.ids.new <- as.character(ids.min:ids.max)
    points.address.new$location.id <- location.ids.new
    points.address.new$location.source <- points.source
    points.address.new$location.type<-'points'
    points.address.new <- points.address.new[,(names(points.address)), with=FALSE]
  } else {
    setkey(points.address.new, lat, long)
    points.address.new <- points.address.new[, location.id:=.GRP, by=key(points.address.new)]
    points.address.new$location.source<-points.source
  }
  # Combine the two
  points.address <- rbindlist(list(points.address, points.address.new), use.names=TRUE, fill=TRUE)
  points.address <- points.address[, street.num:=as.numeric(street.num)]
  setkeyv(points.address, names(points.address)[names(points.address)!='location.id'])
  points.address<-unique(points.address)
  save(points.address, file=l.path$clean$address)
  return(points.address)
}
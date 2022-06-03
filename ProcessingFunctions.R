#This script contains most of the functions needed to estimate loss rates from the catchment above a location

#' A function to find the catchment upstream of a reach
#'
#'CatchmentCreator() creates a vector of the reach ID numbers (nzsegment, nzsgmnt or nzseg_v3 attribute) downstream of a reach. It uses the REC V2.4, or Environment Southland's REC3 network. See \href{https://niwa.co.nz/freshwater-and-estuaries/management-tools/river-environment-classification-0}{NIWA REC V2} for details about REC V2.
#'@param RECV2Watersheds A simple features (sf)  polygon representation of the REC V2 watersheds), TO_NODE and FROM_NODE (or FROM_NO) attributes.
#'@parma RECV2Reaches A dataframe of the RECV2 database, with at least the nzsegment, to_node and from_node attributes
#'@param SourceReach The reach number, i.e. nzsegment attribute, of the reach for which the catchment is required
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A simple feature (sf) polygon of the catchment
#'@keywords REC River Environment Classification
#'@export
CatchmentCreator <- function(Watersheds=RECV2Watersheds,ReachNetwork=RECV2ReachNetwork,OutletReach=7239110){ #The reach ID is for the Manawatu at Teachers College water quality reporting site
  print(paste("Creating catchment for:",OutletReach))
  #Initialise a vector of reach ID's within the catchment of the outlet
  CatchmentReaches <- OutletReach
  
  #Identify the row index of the outlet reach
  OutletReachIndex<-which(ReachNetwork$nzsegment==OutletReach)
  
  #Identify the reach's immediately upstream of the outlet reach
  upstream_indices<-which(ReachNetwork$TO_NODE==ReachNetwork$FROM_NODE[OutletReachIndex])
  
  #Sequentially work upstream
  while (length(upstream_indices)>0) {
    
    CatchmentReaches <- c(CatchmentReaches,ReachNetwork$nzsegment[upstream_indices])
    upstream_indices<-which(ReachNetwork$TO_NODE %in% ReachNetwork$FROM_NODE[upstream_indices])
    #downstream_index<-next_downstream_index
  } #end of while

  #Extract the watersheds of interest
  CatchmentWaterSheds <- Watersheds[Watersheds$nzsegment %in% CatchmentReaches,]
  #browser()
  #Check for validity, and make valid if they are not
  #InvalidPolygons <- which(!st_is_valid(CatchmentWaterSheds))
  #CatchmentWaterSheds[InvalidPolygons,] <- st_make_valid(CatchmentWaterSheds[InvalidPolygons,])
  
  #Merge into a single polygon
  Catchment <- st_union(CatchmentWaterSheds)
  Catchment <- sf::st_as_sf(Catchment)
  Catchment$nzsegment <- OutletReach
  
  #Test for validity
  try(if(!st_is_valid(Catchment)) stop("created catchment polygon is not valid"))
  
  return(Catchment)
}

#' A function to reclassify LCDBV5
#'
#'LCDBV5Reclassify() creates a vector of the reach ID numbers (nzsegment, nzsgmnt or nzseg_v3 attribute) downstream of a reach. It uses the REC V2.4, or Environment Southland's REC3 network. See \href{https://niwa.co.nz/freshwater-and-estuaries/management-tools/river-environment-classification-0}{NIWA REC V2} for details about REC V2.
#'@param LCDBV5 A simple features (sf)  polygon representation of the LCDBV5.
#'@parma ReclassTable A dataframe of the RECV2 database, with at least the nzsegment, to_node and from_node attributes
#'@param SourceReach The reach number, i.e. nzsegment attribute, of the reach for which the catchment is required
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A simple feature (sf) polygon of the catchment
#'@keywords REC River Environment Classification
#'@export
LCDBV5Reclassify <- function(LCDBV5 = LCDBV5, ReclassTable = )
